%% iterator kernels for aot and the interpreter; exports may answer miss
-module(arc_rt_lang_ffi).
-export([plain_iter_record/2, array_iter_start/2, array_iter_next/2, is_array_iter/1,
         array_iter_parts/1, array_iter_record/3, array_iter_proto/2,
         array_spread/2, iter_step/2, for_of_next/2, unpack_array/3,
         unpack_array/4]).


-include("arc_rt_layout.hrl").

-define(NAMED_KEY(Name), {?KEY_NAMED, <<Name>>}).

plain_iter_record(St, {?HANDLE_TAG, Id}) ->
    Cells = element(?STORE_CELLS, element(?AGENT_STORE, St)),
    case arc_rt_arena_ffi:get(Id, Cells) of
        {?SOBJECT_TAG, ?ORDINARY, _,
         #{?NAMED_KEY("done") := DoneP, ?NAMED_KEY("iterator") := IterP, ?NAMED_KEY("next") := NextP},
         _, _, _}
          when element(1, DoneP) =:= ?DATAPROPERTY_TAG,
               element(1, IterP) =:= ?DATAPROPERTY_TAG,
               element(1, NextP) =:= ?DATAPROPERTY_TAG ->
            Iter = element(?DATAPROPERTY_VALUE, IterP),
            Next = element(?DATAPROPERTY_VALUE, NextP),
            Done = arc_rt_val_ffi:to_boolean(element(?DATAPROPERTY_VALUE, DoneP)),
            {plain_record, Done, {?ITERATORRECORD_TAG, Iter, Next},
             native(Cells, Iter, Next)};
        _ -> record_miss
    end;
plain_iter_record(_, _) -> record_miss.

native(Cells, {?HANDLE_TAG, IId} = IterH, {?HANDLE_TAG, NId}) ->
    case arc_rt_arena_ffi:probe(NId, Cells) of
        NCell when element(1, NCell) =:= ?SOBJECT_TAG ->
            case element(?SOBJECT_KIND, NCell) of
                {?NATIVEFN_TAG, {?ITERATORN_TAG, Which}, _, _, _} ->
                    {native_next, Which, IterH};
                {?NATIVEFN_TAG, ?TOKEN_GENERATOR_NEXT, _, _, _} ->
                    case arc_rt_arena_ffi:probe(IId, Cells) of
                        ICell when element(1, ICell) =:= ?SOBJECT_TAG,
                                   element(1, element(?SOBJECT_KIND, ICell))
                                       =:= ?GENERATOROBJ_TAG ->
                            {native_generator,
                             element(?GENERATOROBJ_DATA,
                                     element(?SOBJECT_KIND, ICell))};
                        _ -> native_miss
                    end;
                _ -> native_miss
            end;
        _ -> native_miss
    end;
native(_, _, _) -> native_miss.

%% the store with the iterator in scope moved to Index
-define(AT_INDEX(Index),
        setelement(?STORE_CELLS, Store,
                   arc_rt_arena_ffi:set(
                     IterId,
                     setelement(?SOBJECT_KIND, IterCell, {Tag, Target, Index, Kind}),
                     Cells))).
-define(ADVANCE(Index, Done, V), {advanced, Done, V, ?AT_INDEX(Index)}).
-define(ADVANCE_PAIR(Index, K, V), {pair_advanced, K, V, ?AT_INDEX(Index)}).

%% steps a native iterator object in place when that observes nothing; -1 is done
iter_step(Store, {?HANDLE_TAG, RecId}) ->
    Cells = element(?STORE_CELLS, Store),
    case arc_rt_arena_ffi:get(RecId, Cells) of
        {?SOBJECT_TAG, ?ORDINARY, _,
         #{?NAMED_KEY("iterator") := IP, ?NAMED_KEY("next") := NP}, _, _, _}
          when element(1, IP) =:= ?DATAPROPERTY_TAG,
               element(1, NP) =:= ?DATAPROPERTY_TAG ->
            case {element(?DATAPROPERTY_VALUE, NP), element(?DATAPROPERTY_VALUE, IP)} of
                {{?HANDLE_TAG, NextId}, {?HANDLE_TAG, IterId}} ->
                    case arc_rt_arena_ffi:get(IterId, Cells) of
                        IterCell when element(1, IterCell) =:= ?SOBJECT_TAG ->
                            iter_step_with(
                              native_token(arc_rt_arena_ffi:get(NextId, Cells)),
                              element(?SOBJECT_KIND, IterCell),
                              Store, Cells, IterId, IterCell);
                        _ -> iter_miss
                    end;
                _ -> iter_miss
            end;
        _ -> iter_miss
    end;
iter_step(_, _) -> iter_miss.

iter_step_with(?TOKEN_GENERATOR_NEXT, {?GENERATOROBJ_TAG, DataH}, _, _, _, _) ->
    {resume_generator, DataH};
iter_step_with(?TOKEN_ARRAY_ITER_NEXT, {?ARRAYITERATOR_TAG, _, Index, _}, Store, _, _, _)
  when Index < 0 ->
    {advanced, true, undefined, Store};
iter_step_with(?TOKEN_MAP_ITER_NEXT, {?MAPITERATOR_TAG, _, Index, _}, Store, _, _, _)
  when Index < 0 ->
    {advanced, true, undefined, Store};
iter_step_with(?TOKEN_SET_ITER_NEXT, {?SETITERATOR_TAG, _, Index, _}, Store, _, _, _)
  when Index < 0 ->
    {advanced, true, undefined, Store};
iter_step_with(?TOKEN_ARRAY_ITER_NEXT,
               {?ARRAYITERATOR_TAG = Tag, {?HANDLE_TAG, T} = Target, Index, Kind},
               Store, Cells, IterId, IterCell) ->
    case arc_rt_arena_ffi:get(T, Cells) of
        {?SOBJECT_TAG, {?ARRAYOBJ_TAG, Len}, _, _, _, _, _} when Index >= Len ->
            ?ADVANCE(-1, true, undefined);
        {?SOBJECT_TAG, {?ARRAYOBJ_TAG, _}, _, _, _, _, _} when Kind =:= ?ARRAYITER_KEYS ->
            ?ADVANCE(Index + 1, false, Index);
        {?SOBJECT_TAG, {?ARRAYOBJ_TAG, _}, _, Props, _, Els, _} ->
            case map_size(Props) =/= 0
                 andalso is_map_key({?KEY_INDEX, Index}, Props) of
                true -> iter_miss;
                false ->
                    case elem_at(Els, Index) of
                        ?ELEMS_HOLE -> iter_miss;
                        V when Kind =:= ?ARRAYITER_VALUES -> ?ADVANCE(Index + 1, false, V);
                        V -> ?ADVANCE_PAIR(Index + 1, Index, V)
                    end
            end;
        _ -> iter_miss
    end;
iter_step_with(?TOKEN_MAP_ITER_NEXT,
               {?MAPITERATOR_TAG = Tag, {?HANDLE_TAG, T} = Target, Index, Kind},
               Store, Cells, IterId, IterCell) ->
    case arc_rt_arena_ffi:get(T, Cells) of
        {?SOBJECT_TAG, {?MAPOBJ_TAG, Entries}, _, _, _, _, _} ->
            case arc_ordered_entries_ffi:next_from(Entries, Index) of
                ?NONE -> ?ADVANCE(-1, true, undefined);
                {?SOME, {Next, _, V}} when Kind =:= ?MAPITER_VALUES ->
                    ?ADVANCE(Next, false, V);
                {?SOME, {Next, MK, _}} when Kind =:= ?MAPITER_KEYS ->
                    ?ADVANCE(Next, false, 'arc@rt@types':map_key_to_js(MK));
                {?SOME, {Next, MK, V}} ->
                    ?ADVANCE_PAIR(Next, 'arc@rt@types':map_key_to_js(MK), V)
            end;
        _ -> iter_miss
    end;
iter_step_with(?TOKEN_SET_ITER_NEXT,
               {?SETITERATOR_TAG = Tag, {?HANDLE_TAG, T} = Target, Index, Kind},
               Store, Cells, IterId, IterCell) ->
    case arc_rt_arena_ffi:get(T, Cells) of
        {?SOBJECT_TAG, {?SETOBJ_TAG, Entries}, _, _, _, _, _} ->
            case arc_ordered_entries_ffi:next_from(Entries, Index) of
                ?NONE -> ?ADVANCE(-1, true, undefined);
                {?SOME, {Next, _, V}} when Kind =:= ?SETITER_VALUES ->
                    ?ADVANCE(Next, false, V);
                {?SOME, {Next, _, V}} -> ?ADVANCE_PAIR(Next, V, V)
            end;
        _ -> iter_miss
    end;
iter_step_with(_, _, _, _, _, _) -> iter_miss.

%% unobserved for-of state {arc_iter, Target, Index, NextFn}; strings index by byte
array_iter_start(St, {?HANDLE_TAG, Id} = V) ->
    Realm = element(?AGENT_REALM, St),
    Cells = element(?STORE_CELLS, element(?AGENT_STORE, St)),
    case arc_rt_arena_ffi:get(Id, Cells) of
        {?SOBJECT_TAG, {?ARRAYOBJ_TAG, _}, Proto, _, [], _, _} ->
            pristine(Realm, Cells, V, Proto, ?REALM_ARRAY, ?REALM_ARRAY_ITER_PROTO,
                     ?TOKEN_ARRAY_VALUES, ?TOKEN_ARRAY_ITER_NEXT);
        {?SOBJECT_TAG, {?MAPOBJ_TAG, _}, Proto, _, [], _, _} ->
            pristine(Realm, Cells, V, Proto, ?REALM_MAP, ?REALM_MAP_ITER_PROTO,
                     ?TOKEN_MAP_ENTRIES, ?TOKEN_MAP_ITER_NEXT);
        {?SOBJECT_TAG, {?SETOBJ_TAG, _}, Proto, _, [], _, _} ->
            pristine(Realm, Cells, V, Proto, ?REALM_SET, ?REALM_SET_ITER_PROTO,
                     ?TOKEN_SET_VALUES, ?TOKEN_SET_ITER_NEXT);
        _ -> miss
    end;
array_iter_start(St, S) when ?IS_STR(S) ->
    Realm = element(?AGENT_REALM, St),
    Cells = element(?STORE_CELLS, element(?AGENT_STORE, St)),
    Proto = {?SOME, element(?BUILTINPAIR_PROTOTYPE, element(?REALM_STRING, Realm))},
    pristine(Realm, Cells, S, Proto, ?REALM_STRING, ?REALM_STRING_ITER_PROTO,
             ?TOKEN_STRING_ITER, ?TOKEN_STRING_ITER_NEXT);
array_iter_start(_, _) -> miss.

%% V's @@iterator and the iterator prototype's next are still the intrinsics
pristine(Realm, Cells, V, Proto, Class, IterProto, IterTok, NextTok) ->
    {?HANDLE_TAG, CP} = element(?BUILTINPAIR_PROTOTYPE, element(Class, Realm)),
    {?HANDLE_TAG, IP} = element(IterProto, Realm),
    case Proto =:= {?SOME, {?HANDLE_TAG, CP}} of
        false -> miss;
        true ->
            case {arc_rt_arena_ffi:get(CP, Cells), arc_rt_arena_ffi:get(IP, Cells)} of
                {{?SOBJECT_TAG, _, _, _, Syms, _, _},
                 {?SOBJECT_TAG, _, _, #{?NAMED_KEY("next") := NP}, _, _, _}}
                  when element(1, NP) =:= ?DATAPROPERTY_TAG ->
                    N = element(?DATAPROPERTY_VALUE, NP),
                    case lists:keyfind(?SYMBOL_ITERATOR, 1, Syms) of
                        {_, VP} when element(1, VP) =:= ?DATAPROPERTY_TAG ->
                            case token_of(Cells, element(?DATAPROPERTY_VALUE, VP)) =:= IterTok
                                 andalso token_of(Cells, N) =:= NextTok of
                                true -> {?ARC_ITER, V, 0, N};
                                false -> miss
                            end;
                        _ -> miss
                    end;
                _ -> miss
            end
    end.

token_of(Cells, {?HANDLE_TAG, Id}) -> native_token(arc_rt_arena_ffi:get(Id, Cells));
token_of(_, _) -> none.

native_token(Cell) -> ?NATIVE_TOKEN(Cell).

elem_at(Els, Idx) -> ?ELEM_AT(Els, Idx).

is_array_iter({?ARC_ITER, _, _, _}) -> true;
is_array_iter(_) -> false.

%% holes and index props go the long way
array_iter_next(Store, {?ARC_ITER, {?HANDLE_TAG, T}, I, _} = R) ->
    case arc_rt_arena_ffi:get(T, element(?STORE_CELLS, Store)) of
        {?SOBJECT_TAG, {?ARRAYOBJ_TAG, Len}, _, _, _, _, _} when I >= Len ->
            {iter_step, true, undefined, undefined};
        {?SOBJECT_TAG, {Tag, Entries}, _, _, _, _, _}
          when Tag =:= ?MAPOBJ_TAG; Tag =:= ?SETOBJ_TAG ->
            case arc_ordered_entries_ffi:next_from(Entries, I) of
                ?NONE -> {iter_step, true, undefined, undefined};
                {?SOME, {Next, _, V}} when Tag =:= ?SETOBJ_TAG ->
                    {iter_step, false, V, setelement(3, R, Next)};
                {?SOME, {Next, MK, V}} ->
                    {iter_pair, 'arc@rt@types':map_key_to_js(MK), V,
                     setelement(3, R, Next)}
            end;
        {?SOBJECT_TAG, {?ARRAYOBJ_TAG, _}, _, Props, _, Els, _} ->
            case map_size(Props) =/= 0
                 andalso is_map_key({?KEY_INDEX, I}, Props) of
                true -> iter_miss;
                false ->
                    case elem_at(Els, I) of
                        ?ELEMS_HOLE -> iter_miss;
                        V -> {iter_step, false, V, setelement(3, R, I + 1)}
                    end
            end;
        _ -> iter_miss
    end;
array_iter_next(_, {?ARC_ITER, S, Off, _} = R) when ?IS_STR(S) ->
    case arc_rt_js_string_ffi:text(S) of
        <<_:Off/binary, C/utf8, _/binary>> ->
            Ch = <<C/utf8>>,
            {iter_step, false,
             case C < 16#80 of true -> Ch; false -> arc_rt_js_string_ffi:from_text(Ch) end,
             setelement(3, R, Off + byte_size(Ch))};
        _ -> {iter_step, true, undefined, undefined}
    end;
array_iter_next(_, _) -> iter_miss.

for_of_next(St, {?ARC_ITER, _, _, _} = R) ->
    case array_iter_next(element(?AGENT_STORE, St), R) of
        {iter_step, Done, V, R2} -> {{Done, V, R2}, St};
        {iter_pair, K, V, R2} ->
            {Pair, St2} = 'arc@rt@obj':new_array(St, [K, V]),
            {{false, Pair, R2}, St2};
        iter_miss -> 'arc@rt@lang':array_iter_next_general(St, R)
    end;
for_of_next(St, undefined) -> {{true, undefined, undefined}, St};
for_of_next(St, R) ->
    case iter_step(element(?AGENT_STORE, St), R) of
        {advanced, false, V, Store} -> {{false, V, R}, setelement(?AGENT_STORE, St, Store)};
        {advanced, true, V, Store} ->
            {{true, V, undefined}, setelement(?AGENT_STORE, St, Store)};
        {pair_advanced, K, V, Store} ->
            {Pair, St2} = 'arc@rt@obj':new_array(setelement(?AGENT_STORE, St, Store), [K, V]),
            {{false, Pair, R}, St2};
        {resume_generator, DataH} ->
            case 'arc@rt@lang':generator_step(St, R, DataH) of
                {{?SOME, V}, St2} -> {{false, V, R}, St2};
                {?NONE, St2} -> {{true, undefined, undefined}, St2}
            end;
        iter_miss ->
            case 'arc@rt@lang':iter_next(St, R) of
                {{true, V}, St2} -> {{true, V, undefined}, St2};
                {{false, V}, St2} -> {{false, V, R}, St2}
            end
    end.

%% the intrinsic prototype a materialized iterator for this record would get
array_iter_proto(St, R) ->
    element(iter_proto_ix(St, R), element(?AGENT_REALM, St)).

iter_proto_ix(_, {?ARC_ITER, S, _, _}) when ?IS_STR(S) -> ?REALM_STRING_ITER_PROTO;
iter_proto_ix(St, {?ARC_ITER, {?HANDLE_TAG, T}, _, _}) ->
    Cells = element(?STORE_CELLS, element(?AGENT_STORE, St)),
    case element(?SOBJECT_KIND, arc_rt_arena_ffi:get(T, Cells)) of
        {?MAPOBJ_TAG, _} -> ?REALM_MAP_ITER_PROTO;
        {?SETOBJ_TAG, _} -> ?REALM_SET_ITER_PROTO;
        _ -> ?REALM_ARRAY_ITER_PROTO
    end.


array_iter_parts({?ARC_ITER, T, I, N}) -> {T, I, N}.

array_iter_record(T, I, N) -> {?ARC_ITER, T, I, N}.

unpack_array(St, V, N) ->
    case unpack_array(St, V, N, []) of
        miss -> miss;
        L -> list_to_tuple(L)
    end.

%% the first N elements pushed onto Tail, when destructuring observes nothing
unpack_array(St, {?HANDLE_TAG, Id} = V, N, Tail) ->
    Cells = element(?STORE_CELLS, element(?AGENT_STORE, St)),
    case arc_rt_arena_ffi:get(Id, Cells) of
        {?SOBJECT_TAG, {?ARRAYOBJ_TAG, Len}, _, Props, _, Els, _}
          when map_size(Props) =:= 0 ->
            case array_iter_start(St, V) =/= miss
                 andalso lacks_return(
                           Cells,
                           {?SOME, element(?REALM_ARRAY_ITER_PROTO,
                                           element(?AGENT_REALM, St))}) of
                true -> unpack(Els, Len, N - 1, Tail);
                false -> miss
            end;
        _ -> miss
    end;
unpack_array(_, _, _, _) -> miss.

unpack(_, _, I, Acc) when I < 0 -> Acc;
unpack(Els, Len, I, Acc) when I >= Len -> unpack(Els, Len, I - 1, [undefined | Acc]);
unpack(Els, Len, I, Acc) ->
    case elem_at(Els, I) of
        ?ELEMS_HOLE -> miss;
        V -> unpack(Els, Len, I - 1, [V | Acc])
    end.

%% §7.4.11 has no return method to call
lacks_return(_, ?NONE) -> true;
lacks_return(Cells, {?SOME, {?HANDLE_TAG, Id}}) ->
    case arc_rt_arena_ffi:get(Id, Cells) of
        {?SOBJECT_TAG, ?ORDINARY, Proto, Props, _, _, _}
          when not is_map_key(?NAMED_KEY("return"), Props) ->
            lacks_return(Cells, Proto);
        _ -> false
    end.

%% every element of a plain hole-free array, when iterating it observes nothing
array_spread(St, V) ->
    case array_iter_start(St, V) of
        {?ARC_ITER, {?HANDLE_TAG, T}, _, _} ->
            Cells = element(?STORE_CELLS, element(?AGENT_STORE, St)),
            case arc_rt_arena_ffi:get(T, Cells) of
                {?SOBJECT_TAG, {?ARRAYOBJ_TAG, Len}, _, Props, _, Els, _}
                  when map_size(Props) =:= 0 ->
                    dense_list(Els, Len);
                _ -> spread_miss
            end;
        _ -> spread_miss
    end.

dense_list(_, 0) -> {spread, []};
dense_list({?ELEMS_DENSE, A}, Len) ->
    case arc_tree_array_ffi:size(A) of
        Len ->
            L = arc_tree_array_ffi:to_list(A),
            case lists:member(?ELEMS_HOLE, L) of
                true -> spread_miss;
                false -> {spread, L}
            end;
        _ -> spread_miss
    end;
dense_list(_, _) -> spread_miss.
