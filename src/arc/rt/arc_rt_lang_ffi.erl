%% iterator kernels for aot and the interpreter; none, miss or iter_miss decline
-module(arc_rt_lang_ffi).
-export([iter_fast/2, array_iter_start/2, array_iter_next/2, is_array_iter/1,
         array_iter_parts/1, array_iter_record/3, array_iter_proto/2,
         array_spread/2]).

-include("arc_rt_layout.hrl").

-define(K(Name), {?KEY_NAMED, <<Name>>}).

iter_fast(St, {?HANDLE_TAG, Id}) ->
    Data = element(?STORE_DATA, element(?AGENT_STORE, St)),
    case arc_rt_arena_ffi:get(Id, Data) of
        {?SOBJECT_TAG, ?ORDINARY, _,
         #{?K("done") := DoneP, ?K("iterator") := IterP, ?K("next") := NextP},
         _, _, _}
          when element(1, DoneP) =:= ?DATAPROP_TAG,
               element(1, IterP) =:= ?DATAPROP_TAG,
               element(1, NextP) =:= ?DATAPROP_TAG ->
            Iter = element(?DATAPROP_VALUE, IterP),
            Next = element(?DATAPROP_VALUE, NextP),
            Done = arc_rt_val_ffi:to_boolean(element(?DATAPROP_VALUE, DoneP)),
            {?SOME, {Done, {?ITERATOR_RECORD_TAG, Iter, Next},
                     native(Data, Iter, Next)}};
        _ -> ?NONE
    end;
iter_fast(_, _) -> ?NONE.

native(Data, {?HANDLE_TAG, IId} = IterH, {?HANDLE_TAG, NId}) ->
    case arc_rt_arena_ffi:probe(NId, Data) of
        NCell when element(1, NCell) =:= ?SOBJECT_TAG ->
            case element(?SOBJECT_KIND, NCell) of
                {?NATIVEFN_TAG, {iterator_n, Which}, _, _, _} ->
                    {native_next, Which, IterH};
                {?NATIVEFN_TAG, ?TOKEN_GENERATOR_NEXT, _, _, _} ->
                    case arc_rt_arena_ffi:probe(IId, Data) of
                        ICell when element(1, ICell) =:= ?SOBJECT_TAG,
                                   element(1, element(?SOBJECT_KIND, ICell))
                                       =:= ?GENERATOROBJ_TAG ->
                            {native_generator,
                             element(?GENERATOROBJ_DATA,
                                     element(?SOBJECT_KIND, ICell))};
                        _ -> not_native
                    end;
                _ -> not_native
            end;
        _ -> not_native
    end;
native(_, _, _) -> not_native.

-define(ITER_SYM, {well_known_symbol, sym_iterator}).

%% for-of over a plain array or string keeps the iterator on the stack as
%% {arc_iter, Target, Index, NextFn} while nothing can observe the objects;
%% for strings the index is a byte offset
array_iter_start(Agent, {?HANDLE_TAG, Id} = V) ->
    Realm = element(?AGENT_REALM, Agent),
    Data = element(?STORE_DATA, element(?AGENT_STORE, Agent)),
    case arc_rt_arena_ffi:get(Id, Data) of
        {?SOBJECT_TAG, {?ARRAYOBJ_TAG, _}, Proto, _, [], _, _} ->
            pristine(Realm, Data, V, Proto, ?REALM_ARRAY, ?REALM_ARRAY_ITER_PROTO,
                     ?TOKEN_ARRAY_VALUES, ?TOKEN_ARRAY_ITER_NEXT);
        {?SOBJECT_TAG, {?MAPOBJ_TAG, _}, Proto, _, [], _, _} ->
            pristine(Realm, Data, V, Proto, ?REALM_MAP, ?REALM_MAP_ITER_PROTO,
                     ?TOKEN_MAP_ENTRIES, ?TOKEN_MAP_ITER_NEXT);
        {?SOBJECT_TAG, {?SETOBJ_TAG, _}, Proto, _, [], _, _} ->
            pristine(Realm, Data, V, Proto, ?REALM_SET, ?REALM_SET_ITER_PROTO,
                     ?TOKEN_SET_VALUES, ?TOKEN_SET_ITER_NEXT);
        _ -> miss
    end;
array_iter_start(Agent, S) when ?IS_STR(S) ->
    Realm = element(?AGENT_REALM, Agent),
    Data = element(?STORE_DATA, element(?AGENT_STORE, Agent)),
    Proto = {?SOME, element(?PAIR_PROTO, element(?REALM_STRING, Realm))},
    pristine(Realm, Data, S, Proto, ?REALM_STRING, ?REALM_STRING_ITER_PROTO,
             ?TOKEN_STRING_ITER, ?TOKEN_STRING_ITER_NEXT);
array_iter_start(_, _) -> miss.

%% V inherits @@iterator straight from the class prototype and both that and
%% the iterator prototype's next are still the intrinsics
pristine(Realm, Data, V, Proto, Class, IterProto, IterTok, NextTok) ->
    {?HANDLE_TAG, CP} = element(?PAIR_PROTO, element(Class, Realm)),
    {?HANDLE_TAG, IP} = element(IterProto, Realm),
    case Proto =:= {?SOME, {?HANDLE_TAG, CP}} of
        false -> miss;
        true ->
            case {arc_rt_arena_ffi:get(CP, Data), arc_rt_arena_ffi:get(IP, Data)} of
                {{?SOBJECT_TAG, _, _, _, Syms, _, _},
                 {?SOBJECT_TAG, _, _, #{?K("next") := NP}, _, _, _}}
                  when element(1, NP) =:= ?DATAPROP_TAG ->
                    N = element(?DATAPROP_VALUE, NP),
                    case lists:keyfind(?ITER_SYM, 1, Syms) of
                        {_, VP} when element(1, VP) =:= ?DATAPROP_TAG ->
                            case token_of(Data, element(?DATAPROP_VALUE, VP)) =:= IterTok
                                 andalso token_of(Data, N) =:= NextTok of
                                true -> {?ARC_ITER, V, 0, N};
                                false -> miss
                            end;
                        _ -> miss
                    end;
                _ -> miss
            end
    end.

token_of(Data, {?HANDLE_TAG, Id}) -> arc_interp_ffi:native_token(arc_rt_arena_ffi:get(Id, Data));
token_of(_, _) -> none.

is_array_iter({?ARC_ITER, _, _, _}) -> true;
is_array_iter(_) -> false.

%% holes and index props go the long way
array_iter_next(Store, {?ARC_ITER, {?HANDLE_TAG, T}, I, _} = R) ->
    case arc_rt_arena_ffi:get(T, element(?STORE_DATA, Store)) of
        {?SOBJECT_TAG, {?ARRAYOBJ_TAG, Len}, _, _, _, _, _} when I >= Len ->
            {iter_step, true, undefined, undefined};
        {?SOBJECT_TAG, {Tag, Entries}, _, _, _, _, _}
          when Tag =:= ?MAPOBJ_TAG; Tag =:= ?SETOBJ_TAG ->
            case 'arc@internal@ordered_entries':next_from(Entries, I) of
                none -> {iter_step, true, undefined, undefined};
                {some, {Next, _, V}} when Tag =:= ?SETOBJ_TAG ->
                    {iter_step, false, V, setelement(3, R, Next)};
                {some, {Next, MK, V}} ->
                    {iter_pair, 'arc@rt@types':map_key_to_js(MK), V,
                     setelement(3, R, Next)}
            end;
        {?SOBJECT_TAG, {?ARRAYOBJ_TAG, _}, _, Props, _, Els, _} ->
            case map_size(Props) =/= 0
                 andalso is_map_key({?KEY_INDEX, I}, Props) of
                true -> iter_miss;
                false ->
                    case arc_interp_ffi:iter_elem(Els, I) of
                        ?ELEMS_HOLE -> iter_miss;
                        V -> {iter_step, false, V, setelement(3, R, I + 1)}
                    end
            end;
        _ -> iter_miss
    end;
array_iter_next(_, {?ARC_ITER, S, Off, _} = R) when ?IS_STR(S) ->
    case arc_rt_str_ffi:bin(S) of
        <<_:Off/binary, C/utf8, _/binary>> ->
            Ch = <<C/utf8>>,
            {iter_step, false,
             case C < 16#80 of true -> Ch; false -> arc_rt_str_ffi:mk(Ch) end,
             setelement(3, R, Off + byte_size(Ch))};
        _ -> {iter_step, true, undefined, undefined}
    end;
array_iter_next(_, _) -> iter_miss.

%% the intrinsic prototype a materialized iterator for this record would get
array_iter_proto(Agent, R) ->
    element(iter_proto_ix(Agent, R), element(?AGENT_REALM, Agent)).

iter_proto_ix(_, {?ARC_ITER, S, _, _}) when ?IS_STR(S) -> ?REALM_STRING_ITER_PROTO;
iter_proto_ix(Agent, {?ARC_ITER, {?HANDLE_TAG, T}, _, _}) ->
    Data = element(?STORE_DATA, element(?AGENT_STORE, Agent)),
    case element(?SOBJECT_KIND, arc_rt_arena_ffi:get(T, Data)) of
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
            Data = element(?STORE_DATA, element(?AGENT_STORE, Agent)),
            case arc_rt_arena_ffi:get(T, Data) of
                {?SOBJECT_TAG, {?ARRAYOBJ_TAG, Len}, _, Props, _, Els, _}
                  when map_size(Props) =:= 0 ->
                    dense_list(Els, Len);
                _ -> none
            end;
        _ -> none
    end.

dense_list(_, 0) -> {some, []};
dense_list({?ELEMS_DENSE, A}, Len) ->
    case arc_tree_array_ffi:size(A) of
        Len ->
            L = arc_tree_array_ffi:to_list(A),
            case lists:member(?ELEMS_HOLE, L) of
                true -> none;
                false -> {some, L}
            end;
        _ -> none
    end;
dense_list(_, _) -> none.
