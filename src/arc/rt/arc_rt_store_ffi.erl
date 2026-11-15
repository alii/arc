-module(arc_rt_store_ffi).
-export([throw/2, is_handle/1, as_object_key/1,
         cell_get/2, cell_new/2, cell_set/3, box_get/2, box_set/3, set_stored/3,
         global_written/1, new_iter_result/3, iter_result_offsets/0,
         shared_start/1, with_room/1, weighed/2]).

-include("arc_rt_layout.hrl").
-include("arc_rt_arena.hrl").

%% in words
-define(ROOM, 46422).
-define(IS_TABLE(Tag), (Tag =:= ?MAPOBJ_TAG orelse Tag =:= ?SETOBJ_TAG)).

cell_get(St, {?HANDLE_TAG, Id}) ->
    case arena_get(Id, element(?AGENT_CELLS, St)) of
        ?STORE_FREE_CELL -> dangling(<<"cell_get">>);
        Cell -> Cell
    end.

cell_new(St, Cell) when tuple_size(St) =:= ?AGENT_SIZE ->
    Id = element(?AGENT_NEXT_ID, St),
    St1 = ?ALLOC_CELL(St, element(?AGENT_CELLS, St), Id, Cell),
    case Cell of
        {?SOBJECT_TAG, {?ARRAYOBJ_TAG, Len}, _, _, _, {?ELEMS_DENSE, _}, _} when Len > 64 ->
            {{?HANDLE_TAG, Id}, weighed(St1, Len)};
        {?SOBJECT_TAG, {?ARRAYBUFFEROBJ_TAG, {bytes, Bytes, _}}, _, _, _, _, _} ->
            {{?HANDLE_TAG, Id}, weighed(St1, byte_size(Bytes) bsr 3)};
        {?SOBJECT_TAG, {Tag, Table}, _, _, _, _, _}
          when ?IS_TABLE(Tag), element(?ORDERED_NEXT_SEQ, Table) > 0 ->
            {{?HANDLE_TAG, Id}, weighed(St1, element(?ORDERED_NEXT_SEQ, Table) bsl 2)};
        _ -> {{?HANDLE_TAG, Id}, St1}
    end.

weighed(St, Elements) when tuple_size(St) =:= ?AGENT_SIZE ->
    setelement(?AGENT_GC_AT, St,
               element(?AGENT_GC_AT, St) - (Elements bsr ?ELEMENTS_PER_CELL_BITS)).

%% a persistent term is on no process's heap, so neither collecting nor sending
%% copies it. new code for anything that may have made it makes it again
shared_start(Make) ->
    case erlang:system_info(machine) of
        "BEAM" ->
            case shared() of
                none -> share_start(Make);
                St -> St
            end;
        %% atomvm has no such thing
        _ -> Make()
    end.

shared() ->
    case persistent_term:get(?MODULE, none) of
        {Modules, Versions, St} ->
            case versions(Modules) of
                Versions -> St;
                _ -> none
            end;
        none -> none
    end.

%% putting one over another costs every process a collection, so one that some
%% other process has put there in the meantime stays
share_start(Make) ->
    St = set_stored(?STORE_OPS, setelement(?AGENT_HOOKS, Make(), none), none),
    case shared() of
        none ->
            Modules = [M || M <- erlang:loaded(), is_runtime(atom_to_list(M))],
            persistent_term:put(?MODULE, {Modules, versions(Modules), St}),
            element(3, persistent_term:get(?MODULE));
        Theirs -> Theirs
    end.

%% not the modules scripts compile to, which come and go
is_runtime("arc@" ++ _) -> true;
is_runtime("arc_" ++ Name) -> lists:suffix("_ffi", Name);
is_runtime(_) -> false.

versions(Modules) -> [erlang:module_loaded(M) andalso M:module_info(md5) || M <- Modules].

%% running code makes short-lived terms without end, and a heap sized for what
%% little it keeps would be collected every few hundred of them
with_room(Run) ->
    try process_flag(min_heap_size, ?ROOM) of
        Before when Before > ?ROOM ->
            process_flag(min_heap_size, Before),
            Run();
        Before ->
            try Run()
            after process_flag(min_heap_size, Before)
            end
    catch
        %% not a flag atomvm has
        error:badarg -> Run()
    end.

new_iter_result(St, V, Done) -> cell_new(St, ?ITER_RESULT(St, V, Done)).

iter_result_offsets() -> ?ITER_RESULT_OFFSETS.

%% drops whatever was remembered of the cell as it stood
cell_set(St, {?HANDLE_TAG, Id}, Cell) when tuple_size(St) =:= ?AGENT_SIZE ->
    St1 = with_cells(St, arc_rt_arena_ffi:set(Id, Cell, element(?AGENT_CELLS, St))),
    Store = element(?AGENT_STORE, St),
    case Cell of
        {?SOBJECT_TAG, ?GLOBALOBJ, _, _, _, _, _} -> global_written(forget(St1, Store, Id));
        {?SOBJECT_TAG, {?ARRAYOBJ_TAG, Len}, _, _, _, {?ELEMS_DENSE, _}, _} when Len > 64 ->
            St2 = case arena_get(Id, element(?AGENT_CELLS, St)) of
                {?SOBJECT_TAG, {?ARRAYOBJ_TAG, Was}, _, _, _, {?ELEMS_DENSE, _}, _} ->
                    weighed(St1, max(Len - Was, 0));
                _ -> weighed(St1, Len)
            end,
            forget(St2, Store, Id);
        {?SOBJECT_TAG, {Tag, Table}, _, _, _, _, _} when ?IS_TABLE(Tag) ->
            St2 = case arena_get(Id, element(?AGENT_CELLS, St)) of
                {?SOBJECT_TAG, {Tag, Was}, _, _, _, _, _} -> ?TABLE_GROWN(St1, Was, Table);
                _ -> St1
            end,
            forget(St2, Store, Id);
        _ -> forget(St1, Store, Id)
    end.

forget(St, Store, Id)
  when not is_map_key(Id, element(?STORE_PLAIN_WRITE_PROTOS, Store)),
       not is_map_key(Id, element(?STOREMETA_PLAIN_INDEX_PROTOS, element(?STORE_META, Store))) ->
    St;
forget(St, Store, Id) ->
    Store1 = case is_map_key(Id, element(?STORE_PLAIN_WRITE_PROTOS, Store)) of
        true -> setelement(?STORE_PLAIN_WRITE_PROTOS, Store, #{});
        false -> Store
    end,
    Meta = element(?STORE_META, Store),
    Store2 = case is_map_key(Id, element(?STOREMETA_PLAIN_INDEX_PROTOS, Meta)) of
        true ->
            setelement(?STORE_META, Store1,
                       setelement(?STOREMETA_PLAIN_INDEX_PROTOS, Meta, #{}));
        false -> Store1
    end,
    setelement(?AGENT_STORE, St, Store2).

global_written(St) when tuple_size(St) =:= ?AGENT_SIZE ->
    case element(?AGENT_STORE, St) of
        Store when tuple_size(Store) =:= ?STORE_SIZE ->
            Epoch = element(?STORE_GLOBAL_EPOCH, Store) + 1,
            setelement(?AGENT_GLOBALS,
                       setelement(?AGENT_STORE, St,
                                  setelement(?STORE_GLOBAL_EPOCH, Store, Epoch)),
                       #{})
    end.

set_stored(Field, St, V) when tuple_size(St) =:= ?AGENT_SIZE ->
    setelement(?AGENT_STORE, St, setelement(Field, element(?AGENT_STORE, St), V)).

box_get(St, {?HANDLE_TAG, Id}) ->
    case arena_get(Id, element(?AGENT_CELLS, St)) of
        ?STORE_FREE_CELL -> dangling(<<"box_get">>);
        {?SBOX_TAG, V} -> V
    end.

dangling(Who) ->
    erlang:error(#{gleam_error => panic, message =>
        <<Who/binary, ": dangling Handle (use-after-free)">>}).

throw(St, V) -> erlang:error(?JS_THROW(St, V)).

is_handle({?HANDLE_TAG, N}) when is_integer(N) -> true;
is_handle(_) -> false.

as_object_key({?STRINGKEY_TAG, _} = K) -> K;
as_object_key({?SYMBOLKEY_TAG, _} = K) -> K;
as_object_key(K) -> {?STRINGKEY_TAG, K}.

%% a box is never a prototype or a global object, so nothing watches this write
box_set(St, {?HANDLE_TAG, Id}, V) when tuple_size(St) =:= ?AGENT_SIZE ->
    Cells = arc_rt_arena_ffi:set(Id, {?SBOX_TAG, V}, element(?AGENT_CELLS, St)),
    with_cells(St, Cells).

with_cells(St, Cells) when tuple_size(St) =:= ?AGENT_SIZE ->
    setelement(?AGENT_CELLS, St, Cells).
