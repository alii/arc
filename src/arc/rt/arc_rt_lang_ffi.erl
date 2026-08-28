%% aot iterator record fast read, none falls back to the gleam path
-module(arc_rt_lang_ffi).
-export([iter_fast/2]).

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
            {?SOME, {Done, {iterator_record, Iter, Next},
                     native(Data, Iter, Next)}};
        _ -> ?NONE
    end;
iter_fast(_, _) -> ?NONE.

native(Data, {?HANDLE_TAG, IId} = IterH, {?HANDLE_TAG, NId}) ->
    case arc_rt_arena_ffi:probe(NId, Data) of
        NSlot when element(1, NSlot) =:= ?SOBJECT_TAG ->
            case element(?SOBJECT_KIND, NSlot) of
                {?KNATIVE_TAG, {iterator_n, Which}, _, _, _} ->
                    {native_next, Which, IterH};
                {?KNATIVE_TAG, ?TOKEN_GENERATOR_NEXT, _, _, _} ->
                    case arc_rt_arena_ffi:probe(IId, Data) of
                        ISlot when element(1, ISlot) =:= ?SOBJECT_TAG,
                                   element(1, element(?SOBJECT_KIND, ISlot))
                                       =:= ?GENERATOROBJ_TAG ->
                            {native_generator,
                             element(?GENERATOROBJ_DATA,
                                     element(?SOBJECT_KIND, ISlot))};
                        _ -> not_native
                    end;
                _ -> not_native
            end;
        _ -> not_native
    end;
native(_, _, _) -> not_native.
