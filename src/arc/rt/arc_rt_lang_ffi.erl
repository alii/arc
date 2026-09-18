%% iterator kernels for aot and the interpreter; exports may answer miss
-module(arc_rt_lang_ffi).
-export([plain_iter_record/2, array_iter_start/2, array_iter_next/2, is_array_iter/1,
         array_iter_parts/1, array_iter_record/3, array_iter_proto/2,
         array_spread/2]).

-include("arc_rt_layout.hrl").

-define(K(Name), {?KEY_NAMED, <<Name>>}).

plain_iter_record(St, {?HANDLE_TAG, Id}) ->
    Cells = element(?STORE_CELLS, element(?AGENT_STORE, St)),
    case arc_rt_arena_ffi:get(Id, Cells) of
        {?SOBJECT_TAG, ?ORDINARY, _,
         #{?K("done") := DoneP, ?K("iterator") := IterP, ?K("next") := NextP},
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

%% for-of over a plain array or string keeps the iterator on the stack as
%% {arc_iter, Target, Index, NextFn} while nothing can observe the objects;
%% for strings the index is a byte offset
array_iter_start(Agent, {?HANDLE_TAG, Id} = V) ->
    Realm = element(?AGENT_REALM, Agent),
    Cells = element(?STORE_CELLS, element(?AGENT_STORE, Agent)),
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
array_iter_start(Agent, S) when ?IS_STR(S) ->
    Realm = element(?AGENT_REALM, Agent),
    Cells = element(?STORE_CELLS, element(?AGENT_STORE, Agent)),
    Proto = {?SOME, element(?BUILTINPAIR_PROTO, element(?REALM_STRING, Realm))},
    pristine(Realm, Cells, S, Proto, ?REALM_STRING, ?REALM_STRING_ITER_PROTO,
             ?TOKEN_STRING_ITER, ?TOKEN_STRING_ITER_NEXT);
array_iter_start(_, _) -> miss.

%% V inherits @@iterator straight from the class prototype and both that and
%% the iterator prototype's next are still the intrinsics
pristine(Realm, Cells, V, Proto, Class, IterProto, IterTok, NextTok) ->
    {?HANDLE_TAG, CP} = element(?BUILTINPAIR_PROTO, element(Class, Realm)),
    {?HANDLE_TAG, IP} = element(IterProto, Realm),
    case Proto =:= {?SOME, {?HANDLE_TAG, CP}} of
        false -> miss;
        true ->
            case {arc_rt_arena_ffi:get(CP, Cells), arc_rt_arena_ffi:get(IP, Cells)} of
                {{?SOBJECT_TAG, _, _, _, Syms, _, _},
                 {?SOBJECT_TAG, _, _, #{?K("next") := NP}, _, _, _}}
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
            case 'arc@internal@ordered_entries':next_from(Entries, I) of
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
    case arc_rt_js_string_ffi:bin(S) of
        <<_:Off/binary, C/utf8, _/binary>> ->
            Ch = <<C/utf8>>,
            {iter_step, false,
             case C < 16#80 of true -> Ch; false -> arc_rt_js_string_ffi:mk(Ch) end,
             setelement(3, R, Off + byte_size(Ch))};
        _ -> {iter_step, true, undefined, undefined}
    end;
array_iter_next(_, _) -> iter_miss.

%% the intrinsic prototype a materialized iterator for this record would get
array_iter_proto(Agent, R) ->
    element(iter_proto_ix(Agent, R), element(?AGENT_REALM, Agent)).

iter_proto_ix(_, {?ARC_ITER, S, _, _}) when ?IS_STR(S) -> ?REALM_STRING_ITER_PROTO;
iter_proto_ix(Agent, {?ARC_ITER, {?HANDLE_TAG, T}, _, _}) ->
    Cells = element(?STORE_CELLS, element(?AGENT_STORE, Agent)),
    case element(?SOBJECT_KIND, arc_rt_arena_ffi:get(T, Cells)) of
        {?MAPOBJ_TAG, _} -> ?REALM_MAP_ITER_PROTO;
        {?SETOBJ_TAG, _} -> ?REALM_SET_ITER_PROTO;
        _ -> ?REALM_ARRAY_ITER_PROTO
    end.


array_iter_parts({?ARC_ITER, T, I, N}) -> {T, I, N}.

array_iter_record(T, I, N) -> {?ARC_ITER, T, I, N}.

%% every element of a plain hole-free array, when iterating it observes nothing
array_spread(Agent, V) ->
    case array_iter_start(Agent, V) of
        {?ARC_ITER, {?HANDLE_TAG, T}, _, _} ->
            Cells = element(?STORE_CELLS, element(?AGENT_STORE, Agent)),
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
