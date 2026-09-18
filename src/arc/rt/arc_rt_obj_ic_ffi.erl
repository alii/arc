%% site ic kernels called only by aot emitted code; exports may answer miss
-module(arc_rt_obj_ic_ffi).
-export([t_set_named_many/5, t_set_named_init_ic/6, t_new_object_props/3,
         t_set_named_ic/6, t_get_named_ic/4, t_get_named_ic_shaped/4,
         t_global_get_ic/3, t_global_get_ic_fill/3]).

-include("arc_rt_layout.hrl").

-compile({inline, [slot_offset/3]}).

-define(IC_GLOBAL_REFILLS, 16).

%% global data property cache, valid while the epoch holds, keyed by site and name
t_global_get_ic(St, KeyBin, Site) ->
    Store = element(?AGENT_STORE, St),
    case element(?STORE_ICS, Store) of
        #{Site := {?IC_GLOBAL, KeyBin, Epoch, V, _}}
          when Epoch =:= element(?STORE_GLOBAL_EPOCH, Store) ->
            V;
        #{Site := ?IC_OFF} -> arc_rt_obj_ffi:t_global_peek(St, KeyBin);
        _ -> miss
    end.

t_global_get_ic_fill(St, KeyBin, Site) when tuple_size(St) =:= ?AGENT_SIZE ->
    {V, St1} = arc_rt_obj_ffi:t_global_get(St, KeyBin),
    case element(?AGENT_STORE, St1) of
        Store when tuple_size(Store) =:= ?STORE_SIZE ->
            Ics = element(?STORE_ICS, Store),
            N = case Ics of
                #{Site := {?IC_GLOBAL, KeyBin, _, _, N0}} -> N0 + 1;
                #{Site := _} -> ?IC_GLOBAL_REFILLS + 1;
                _ -> 0
            end,
            {?HANDLE_TAG, GId} = element(?REALM_GLOBAL, element(?AGENT_REALM, St1)),
            Cell = arc_rt_arena_ffi:get(GId, element(?STORE_CELLS, Store)),
            Entry = case N < ?IC_GLOBAL_REFILLS andalso global_plain(Cell, KeyBin, V) of
                true -> {?IC_GLOBAL, KeyBin, element(?STORE_GLOBAL_EPOCH, Store), V, N};
                false -> ?IC_OFF
            end,
            case N > ?IC_GLOBAL_REFILLS of
                true -> {V, St1};
                false ->
                    {V, setelement(?AGENT_STORE, St1,
                                   setelement(?STORE_ICS, Store, Ics#{Site => Entry}))}
            end
    end.

global_plain(Cell, KeyBin, V)
  when tuple_size(Cell) =:= ?SOBJECT_SIZE, element(1, Cell) =:= ?SOBJECT_TAG ->
    case element(?SOBJECT_PROPS, Cell) of
        #{{?KEY_NAMED, KeyBin} := Prop} ->
            element(1, Prop) =:= ?DATAPROPERTY_TAG
                andalso element(?DATAPROPERTY_VALUE, Prop) =:= V;
        _ -> false
    end;
global_plain(_, _, _) -> false.

%% shaped receivers only, this reads
t_get_named_ic_shaped(St, {?HANDLE_TAG, Id}, KeyBin, Site) ->
    Store = element(?AGENT_STORE, St),
    case element(?STORE_ICS, Store) of
        #{Site := {?IC_READ, KeyBin, Offs}} ->
            case arc_rt_arena_ffi:get(Id, element(?STORE_CELLS, Store)) of
                {?SSHAPEDOBJECT_TAG, Sid, _, Slots, _} ->
                    case Offs of
                        #{Sid := Off} -> ?SLOT_AT(Slots, Off);
                        _ -> miss
                    end;
                _ -> miss
            end;
        _ -> miss
    end;
t_get_named_ic_shaped(_, _, _, _) -> miss.

%% bare value or miss; on miss arc_rt_obj_ffi:t_get_named fills the ic
t_get_named_ic(St, {?HANDLE_TAG, Id}, KeyBin, Site) ->
    Store = element(?AGENT_STORE, St),
    Cell = arc_rt_arena_ffi:get(Id, element(?STORE_CELLS, Store)),
    case element(1, Cell) of
        ?SSHAPEDOBJECT_TAG ->
            case element(?STORE_ICS, Store) of
                #{Site := {?IC_READ, KeyBin, Offs}} ->
                    case Offs of
                        #{element(?SSHAPEDOBJECT_SID, Cell) := Off} ->
                            ?SLOT_AT(element(?SSHAPEDOBJECT_SLOTS, Cell), Off);
                        _ -> miss
                    end;
                _ -> miss
            end;
        ?SOBJECT_TAG ->
            case element(?SOBJECT_PROPS, Cell) of
                #{{?KEY_NAMED, KeyBin} := Prop}
                  when element(1, Prop) =:= ?DATAPROPERTY_TAG ->
                    Kind = element(?SOBJECT_KIND, Cell),
                    case is_atom(Kind)
                         orelse arc_rt_obj_ffi:named_plain(Kind, KeyBin) of
                        true -> element(?DATAPROPERTY_VALUE, Prop);
                        false -> miss
                    end;
                _ ->
                    case element(?SOBJECT_KIND, Cell) of
                        {?ARRAYOBJ_TAG, Len} when KeyBin =:= <<"length">> -> Len;
                        _ -> miss
                    end
            end;
        _ -> miss
    end;
t_get_named_ic(_, S, <<"length">>, _) when ?IS_STR(S) ->
    arc_rt_js_string_ffi:len(S);
t_get_named_ic(_, _, _, _) -> miss.

%% own overwrite only, size guards keep setelement inline
set_named(St, Obj = {?HANDLE_TAG, Id}, KeyBin, V, Strict)
  when tuple_size(St) =:= ?AGENT_SIZE ->
    case element(?AGENT_STORE, St) of
        Store when tuple_size(Store) =:= ?STORE_SIZE ->
            Cells = element(?STORE_CELLS, Store),
            case arc_rt_arena_ffi:get(Id, Cells) of
                Cell when element(1, Cell) =:= ?SSHAPEDOBJECT_TAG,
                          tuple_size(Cell) =:= ?SSHAPEDOBJECT_SIZE ->
                    case slot_offset(Store, Cell, KeyBin) of
                        miss ->
                            arc_rt_obj_ffi:t_set_named(St, Obj, KeyBin, V,
                                                            Strict);
                        Off ->
                            Slots = ?SLOT_SET(element(?SSHAPEDOBJECT_SLOTS, Cell), Off, V),
                            NewCell = setelement(?SSHAPEDOBJECT_SLOTS, Cell, Slots),
                            setelement(?AGENT_STORE, St,
                                       setelement(?STORE_CELLS, Store,
                                                  arc_rt_arena_ffi:set(Id, NewCell, Cells)))
                    end;
                Cell when element(1, Cell) =:= ?SOBJECT_TAG,
                          tuple_size(Cell) =:= ?SOBJECT_SIZE,
                          element(?SOBJECT_KIND, Cell) =:= ?ORDINARY ->
                    Props = element(?SOBJECT_PROPS, Cell),
                    K = {?KEY_NAMED, KeyBin},
                    case Props of
                        #{K := Prop}
                          when element(1, Prop) =:= ?DATAPROPERTY_TAG,
                               tuple_size(Prop) =:= ?DATAPROPERTY_SIZE,
                               element(?DATAPROPERTY_WRITABLE, Prop) =:= true ->
                            NewCell = setelement(?SOBJECT_PROPS, Cell,
                                                 Props#{K := setelement(?DATAPROPERTY_VALUE, Prop, V)}),
                            setelement(?AGENT_STORE, St,
                                       setelement(?STORE_CELLS, Store,
                                                  arc_rt_arena_ffi:set(Id, NewCell, Cells)));
                        _ ->
                            arc_rt_obj_ffi:t_set_named(St, Obj, KeyBin, V,
                                                            Strict)
                    end;
                _ -> arc_rt_obj_ffi:t_set_named(St, Obj, KeyBin, V, Strict)
            end
    end;
set_named(St, Obj, KeyBin, V, Strict) ->
    arc_rt_obj_ffi:t_set_named(St, Obj, KeyBin, V, Strict).

%% own overwrite first, then the site's cached transition for a new key
t_set_named_ic(St, Obj = {?HANDLE_TAG, Id}, KeyBin, V, Strict, Site)
  when tuple_size(St) =:= ?AGENT_SIZE ->
    case element(?AGENT_STORE, St) of
        Store when tuple_size(Store) =:= ?STORE_SIZE ->
            Cells = element(?STORE_CELLS, Store),
            case arc_rt_arena_ffi:get(Id, Cells) of
                Cell when element(1, Cell) =:= ?SSHAPEDOBJECT_TAG,
                          tuple_size(Cell) =:= ?SSHAPEDOBJECT_SIZE ->
                    case slot_offset(Store, Cell, KeyBin) of
                        miss ->
                            shaped_init(St, Store, Cells, Id, Cell, Obj,
                                        [KeyBin], [V], Strict, Site);
                        Off ->
                            Slots = ?SLOT_SET(element(?SSHAPEDOBJECT_SLOTS, Cell), Off, V),
                            NewCell = setelement(?SSHAPEDOBJECT_SLOTS, Cell, Slots),
                            setelement(?AGENT_STORE, St,
                                       setelement(?STORE_CELLS, Store,
                                                  arc_rt_arena_ffi:set(Id, NewCell, Cells)))
                    end;
                _ -> set_named(St, Obj, KeyBin, V, Strict)
            end
    end;
t_set_named_ic(St, Obj, KeyBin, V, Strict, _) ->
    arc_rt_obj_ffi:t_set_named(St, Obj, KeyBin, V, Strict).

slot_offset(_, Cell, KeyBin) ->
    case element(?SSHAPEDOBJECT_OFFSETS, Cell) of
        #{KeyBin := Off} -> Off;
        _ -> miss
    end.

-define(IC_INIT_HOPS, 8).

%% caches a pure append run from one shape, proto chain checked by identity
t_set_named_init_ic(St, Obj = {?HANDLE_TAG, Id}, Keys, Vals, Strict, Site)
  when tuple_size(St) =:= ?AGENT_SIZE ->
    case element(?AGENT_STORE, St) of
        Store when tuple_size(Store) =:= ?STORE_SIZE ->
            Cells = element(?STORE_CELLS, Store),
            case arc_rt_arena_ffi:get(Id, Cells) of
                Cell when element(1, Cell) =:= ?SSHAPEDOBJECT_TAG,
                          tuple_size(Cell) =:= ?SSHAPEDOBJECT_SIZE ->
                    shaped_init(St, Store, Cells, Id, Cell, Obj, Keys, Vals,
                                Strict, Site);
                _ -> t_set_named_many(St, Obj, Keys, Vals, Strict)
            end
    end;
t_set_named_init_ic(St, Obj, Keys, Vals, Strict, _) ->
    t_set_named_many(St, Obj, Keys, Vals, Strict).

shaped_init(St, Store, Cells, Id, Cell, Obj, Keys, Vals, Strict, Site)
  when tuple_size(St) =:= ?AGENT_SIZE, tuple_size(Store) =:= ?STORE_SIZE ->
    Sid = element(?SSHAPEDOBJECT_SID, Cell),
    Proto = element(?SSHAPEDOBJECT_PROTO, Cell),
    case element(?STORE_ICS, Store) of
        #{Site := {?IC_INIT, Sid, _, Blank, Chain}}
          when element(?SSHAPEDOBJECT_PROTO, Blank) =:= Proto ->
            case chain_same(Cells, Proto, Chain) of
                true ->
                    Slots = init_slots(element(?SSHAPEDOBJECT_SLOTS, Cell), Vals),
                    NewCell = setelement(?SSHAPEDOBJECT_SLOTS, Blank, Slots),
                    setelement(?AGENT_STORE, St,
                               setelement(?STORE_CELLS, Store,
                                          arc_rt_arena_ffi:set(Id, NewCell, Cells)));
                false ->
                    init_fill(t_set_named_many(St, Obj, Keys, Vals, Strict),
                              Id, Sid, Proto, Keys, Site)
            end;
        #{Site := _} -> t_set_named_many(St, Obj, Keys, Vals, Strict);
        _ ->
            init_fill(t_set_named_many(St, Obj, Keys, Vals, Strict), Id, Sid,
                      Proto, Keys, Site)
    end.

init_slots({}, Vals) -> list_to_tuple(Vals);
init_slots(Slots, Vals) -> list_to_tuple(tuple_to_list(Slots) ++ Vals).

chain_same(_, ?NONE, []) -> true;
chain_same(Cells, {?SOME, {?HANDLE_TAG, PId}}, [{PId, PCell} | Rest]) ->
    case arc_rt_arena_ffi:get(PId, Cells) of
        PCell -> chain_same(Cells, element(?CELL_PROTO, PCell), Rest);
        _ -> false
    end;
chain_same(_, _, _) -> false.

%% fill only when the run appended every key in order onto sid
init_fill(St, Id, Sid, Proto, Keys, Site)
  when tuple_size(St) =:= ?AGENT_SIZE ->
    case element(?AGENT_STORE, St) of
        Store when tuple_size(Store) =:= ?STORE_SIZE ->
            Cells = element(?STORE_CELLS, Store),
            Cell = arc_rt_arena_ffi:get(Id, Cells),
            Shapes = element(?STORE_SHAPES, Store),
            Ics = element(?STORE_ICS, Store),
            Entry = case element(1, Cell) =:= ?SSHAPEDOBJECT_TAG
                         andalso element(?SSHAPEDOBJECT_PROTO, Cell) =:= Proto
                         andalso appended(Shapes, Sid, Keys) of
                To when is_integer(To), To =:= element(?SSHAPEDOBJECT_SID, Cell) ->
                    case chain_of(Cells, Proto, ?IC_INIT_HOPS, []) of
                        none -> ?IC_OFF;
                        Chain ->
                            Blank = setelement(?SSHAPEDOBJECT_SLOTS, Cell, {}),
                            {?IC_INIT, Sid, To, Blank, Chain}
                    end;
                _ -> ?IC_OFF
            end,
            setelement(?AGENT_STORE, St,
                       setelement(?STORE_ICS, Store, Ics#{Site => Entry}))
    end.

appended(_, Sid, []) -> Sid;
appended(Shapes, Sid, [K | Ks]) ->
    case Shapes of
        #{Sid := Desc} ->
            SlotCount = element(?SHAPE_SLOT_COUNT, Desc),
            case element(?SHAPE_TRANSITIONS, Desc) of
                #{K := To} ->
                    case Shapes of
                        #{To := ToDesc}
                          when element(?SHAPE_SLOT_COUNT, ToDesc) =:= SlotCount + 1 ->
                            appended(Shapes, To, Ks);
                        _ -> false
                    end;
                _ -> false
            end;
        _ -> false
    end.

chain_of(_, ?NONE, _, Acc) -> lists:reverse(Acc);
chain_of(_, _, 0, _) -> none;
chain_of(Cells, {?SOME, {?HANDLE_TAG, PId}}, Fuel, Acc) ->
    case arc_rt_arena_ffi:get(PId, Cells) of
        PCell when element(1, PCell) =:= ?SOBJECT_TAG;
                   element(1, PCell) =:= ?SSHAPEDOBJECT_TAG ->
            chain_of(Cells, element(?CELL_PROTO, PCell), Fuel - 1,
                     [{PId, PCell} | Acc]);
        _ -> none
    end;
chain_of(_, _, _, _) -> none.

t_set_named_many(St, Obj = {?HANDLE_TAG, Id}, Keys, Vals, Strict) ->
    Store = element(?AGENT_STORE, St),
    Cells = element(?STORE_CELLS, Store),
    case arc_rt_arena_ffi:get(Id, Cells) of
        {?SSHAPEDOBJECT_TAG, Sid, P, Slots, _} ->
            shaped_run(St, Store, Cells, Id, Obj, Sid, P, Slots, Keys, Vals,
                       Strict, element(?STORE_SHAPES, Store), false);
        _ -> each_named(St, Obj, Keys, Vals, Strict)
    end;
t_set_named_many(St, Obj, Keys, Vals, Strict) ->
    each_named(St, Obj, Keys, Vals, Strict).

shaped_run(St, Store, Cells, Id, Obj, Sid, P, Slots, [K | Ks], [V | Vs],
           Strict, Shapes, Dirty) ->
    case Shapes of
        #{Sid := Desc} ->
            case element(?SHAPE_OFFSETS, Desc) of
                #{K := Off} ->
                    shaped_run(St, Store, Cells, Id, Obj, Sid, P,
                               ?SLOT_SET(Slots, Off, V), Ks, Vs, Strict,
                               Shapes, true);
                _ ->
                    case element(?SHAPE_TRANSITIONS, Desc) of
                        #{K := To} ->
                            case arc_rt_obj_ffi:named_write_walk(
                                   Cells, Shapes, P, {?KEY_NAMED, K},
                                   ?MAX_PROTO_HOPS) of
                                true ->
                                    shaped_run(St, Store, Cells, Id, Obj, To,
                                               P,
                                               erlang:append_element(Slots,
                                                                     V),
                                               Ks, Vs, Strict, Shapes, true);
                                false ->
                                    St1 = commit(St, Store, Cells, Id, Sid, P,
                                                 Slots, Shapes, Dirty),
                                    each_named(St1, Obj, [K | Ks], [V | Vs],
                                               Strict)
                            end;
                        _ ->
                            St1 = commit(St, Store, Cells, Id, Sid, P, Slots,
                                         Shapes, Dirty),
                            each_named(St1, Obj, [K | Ks], [V | Vs], Strict)
                    end
            end;
        _ ->
            St1 = commit(St, Store, Cells, Id, Sid, P, Slots, Shapes, Dirty),
            each_named(St1, Obj, [K | Ks], [V | Vs], Strict)
    end;
shaped_run(St, Store, Cells, Id, _, Sid, P, Slots, _, _, _, Shapes, Dirty) ->
    commit(St, Store, Cells, Id, Sid, P, Slots, Shapes, Dirty).

commit(St, _, _, _, _, _, _, _, false) -> St;
commit(St, Store, Cells, Id, Sid, P, Slots, Shapes, true)
  when tuple_size(St) =:= ?AGENT_SIZE, tuple_size(Store) =:= ?STORE_SIZE ->
    #{Sid := Desc} = Shapes,
    Cell = {?SSHAPEDOBJECT_TAG, Sid, P, Slots, element(?SHAPE_OFFSETS, Desc)},
    setelement(?AGENT_STORE, St,
               setelement(?STORE_CELLS, Store, arc_rt_arena_ffi:set(Id, Cell, Cells))).

each_named(St, Obj, [K | Ks], [V | Vs], Strict) ->
    each_named(arc_rt_obj_ffi:t_set_named(St, Obj, K, V, Strict), Obj,
               Ks, Vs, Strict);
each_named(St, _, _, _, _) -> St.

t_new_object_props(St, Keys, Vals) ->
    Store = element(?AGENT_STORE, St),
    new_object_props(St, Store, Keys, Vals).

new_object_props(St, Store, Keys, Vals)
  when tuple_size(St) =:= ?AGENT_SIZE, tuple_size(Store) =:= ?STORE_SIZE ->
    Seq = element(?STORE_PROP_SEQ, Store),
    {Props, Seq1} = props_of(Keys, Vals, Seq, []),
    Proto = element(?BUILTINPAIR_PROTO,
                    element(?REALM_OBJECT, element(?AGENT_REALM, St))),
    Cell = {?SOBJECT_TAG, ?ORDINARY, {?SOME, Proto}, Props, [], ?ELEMS_NONE,
            true},
    Id = element(?STORE_NEXT_ID, Store),
    Store1 = ?ALLOC_CELL(Store, element(?STORE_CELLS, Store), Id, Cell),
    Store2 = setelement(?STORE_PROP_SEQ, Store1, Seq1),
    {{?HANDLE_TAG, Id}, setelement(?AGENT_STORE, St, Store2)}.

props_of([K | Ks], [V | Vs], Seq, Acc) ->
    props_of(Ks, Vs, Seq + 1,
             [{{?KEY_NAMED, K}, ?PLAIN_PROPERTY(V, Seq)}
              | Acc]);
props_of([], [], Seq, Acc) -> {maps:from_list(Acc), Seq}.
