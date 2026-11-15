%% threaded code for properties, elements, globals and boxes
-module(arc_interp_thread_props_ffi).
-export([get_field/4, get_field_keep/4, get_local_field/6, get_local_field_keep/6,
         field_call/5, field_call1/6, local_field_call/7,
         put_field/4, put_field_pop/4, put_local_field/7,
         get_elem/2, get_elem_keep/2, get_elem_locals/5, get_elem_post_inc/5,
         put_elem/2, put_elem_pop/2,
         get_global/4, typeof_global/4, put_global/4,
         get_boxed/4, get_env_boxed/4, put_boxed/3, put_env_boxed/3, box_local/2,
         first_fields/7, new_cell/12]).

-include("arc_interp_thread.hrl").

-compile({inline, [named_plain/2, birth_plain/2]}).
named_plain(Kind, K) -> ?NAMED_KEY_IS_PLAIN(Kind, K).

birth_plain(Birth, K) -> ?LAZY_KEY_IS_PLAIN(Birth, K).

%% the first few slots are quicker to find by their literal positions
-define(SLOT(Slots, Off),
        (case Off of
             0 -> element(1, Slots);
             1 -> element(2, Slots);
             2 -> element(3, Slots);
             3 -> element(4, Slots);
             4 -> element(5, Slots);
             _ -> element(Off + 1, Slots)
         end)).

%% a site is built with the shape it has seen and where in that its key is, -1
%% for nowhere
-define(SEEN, Seen, At).
-define(AT_SLOT(I, Found), I -> V = element(I + 1, Slots), Found).

%% §10.1.8.1 ordinary get of key K, whose id is Id, from Recv. Found is what to
%% do with V and Missed what to do when the lookup could be observed; More is
%% whatever those need. ShapeKey and PropText say an entry under Id is K's
-define(GET(Name, Walk, More, ShapeKey, PropText, Found, Missed),
Name(Recv, St, ?ENV, More, Id, K, ?SEEN) ->
    case Recv of
        {?HANDLE_TAG, H} ->
            Cells = ?CELLS(A),
            case arena_get(H, Cells) of
                {?SSHAPEDOBJECT_TAG, Seen, Proto, Slots, _} ->
                    case At of
                        -1 -> Walk(Recv, St, ?ENV, More, Id, K, Proto, Cells, ?MAX_PROTO_HOPS);
                        ?AT_SLOT(0, Found);
                        ?AT_SLOT(1, Found);
                        ?AT_SLOT(2, Found);
                        ?AT_SLOT(3, Found);
                        ?AT_SLOT(4, Found);
                        ?AT_SLOT(5, Found);
                        ?AT_SLOT(6, Found);
                        ?AT_SLOT(7, Found);
                        ?AT_SLOT(8, Found);
                        ?AT_SLOT(9, Found);
                        ?AT_SLOT(10, Found);
                        ?AT_SLOT(11, Found);
                        _ ->
                            V = element(At + 1, Slots),
                            Found
                    end;
                {?SSHAPEDOBJECT_TAG, _, Proto, Slots, Offs} ->
                    case Offs of
                        #{Id := {SK, Off}} when ShapeKey ->
                            V = ?SLOT(Slots, Off),
                            Found;
                        _ -> Walk(Recv, St, ?ENV, More, Id, K, Proto, Cells, ?MAX_PROTO_HOPS)
                    end;
                {?SOBJECT_TAG, Kind, Proto, Props, _, _, _} ->
                    case named_plain(Kind, K) of
                        true ->
                            case Props of
                                #{Id := {Text, {?DATAPROPERTY_TAG, V, _, _, _, _}}}
                                  when PropText ->
                                    Found;
                                #{Id := _} -> Missed;
                                _ ->
                                    Walk(Recv, St, ?ENV, More, Id, K, Proto, Cells,
                                         ?MAX_PROTO_HOPS)
                            end;
                        false ->
                            case Kind of
                                {?ARRAYOBJ_TAG, V} -> Found;
                                _ -> Missed
                            end
                    end;
                _ -> Missed
            end;
        _ when is_binary(Recv), Id =:= ?KEY_ID(?LENGTH_KEY) ->
            V = byte_size(Recv),
            Found;
        {?STR_TAG, _, V, _} when Id =:= ?KEY_ID(?LENGTH_KEY) -> Found;
        _ when ?IS_STR(Recv) ->
            Pair = element(?REALM_STRING, A#agent.realm),
            Walk(Recv, St, ?ENV, More, Id, K,
                 {?SOME, element(?BUILTINPAIR_PROTOTYPE, Pair)}, ?CELLS(A), ?MAX_PROTO_HOPS);
        _ when is_number(Recv) ->
            Pair = element(?REALM_NUMBER, A#agent.realm),
            Walk(Recv, St, ?ENV, More, Id, K,
                 {?SOME, element(?BUILTINPAIR_PROTOTYPE, Pair)}, ?CELLS(A), ?MAX_PROTO_HOPS);
        _ -> Missed
    end).

-define(WALK(Walk, More, ShapeKey, PropText, Found, Missed),
Walk(Recv, St, ?ENV, More, Id, K, From, Cells, Fuel) ->
    case From of
        {?SOME, {?HANDLE_TAG, H}} when Fuel > 1 ->
            case arena_get(H, Cells) of
                {?SOBJECT_TAG, Kind, Proto, Props, _, _, _} ->
                    case named_plain(Kind, K) of
                        true ->
                            case Props of
                                #{Id := {Text, {?DATAPROPERTY_TAG, V, _, _, _, _}}}
                                  when PropText ->
                                    Found;
                                #{Id := _} -> Missed;
                                _ ->
                                    Walk(Recv, St, ?ENV, More, Id, K, Proto, Cells, Fuel - 1)
                            end;
                        false -> Missed
                    end;
                {?SSHAPEDOBJECT_TAG, _, Proto, Slots, Offs} ->
                    case Offs of
                        #{Id := {SK, Off}} when ShapeKey ->
                            V = ?SLOT(Slots, Off),
                            Found;
                        _ -> Walk(Recv, St, ?ENV, More, Id, K, Proto, Cells, Fuel - 1)
                    end;
                _ -> Missed
            end;
        ?NONE ->
            V = undefined,
            Found;
        _ -> Missed
    end).

-define(PUSH_MORE, N, Pc).
-define(CALL_MORE, N, Pc1, Line, Mode, Args).

-define(PUSH_FOUND, ?GO(N, V, St)).
-define(PUSH_MISSED, get_missed(Recv, St, ?ENV, N, Pc, K)).
-define(CALL_FOUND, ?CALLS:call(V, St, ?ENV, N, Pc1, Line, Mode, Args, Recv)).
-define(CALL_MISSED, call_missed(Recv, St, ?ENV, ?CALL_MORE, K)).
-define(SAME_KEY, ?KEY_TEXT(SK) =:= ?KEY_TEXT(K)).
-define(SAME_NAME, Text =:= ?KEY_TEXT(K)).

?GET(get_exact, walk_exact, ?PUSH_MORE, true, true, ?PUSH_FOUND, ?PUSH_MISSED).
?WALK(walk_exact, ?PUSH_MORE, true, true, ?PUSH_FOUND, ?PUSH_MISSED).
?GET(get_hashed, walk_hashed, ?PUSH_MORE, ?SAME_KEY, ?SAME_NAME, ?PUSH_FOUND, ?PUSH_MISSED).
?WALK(walk_hashed, ?PUSH_MORE, ?SAME_KEY, ?SAME_NAME, ?PUSH_FOUND, ?PUSH_MISSED).
?GET(call_exact, call_walk_exact, ?CALL_MORE, true, true, ?CALL_FOUND, ?CALL_MISSED).
?WALK(call_walk_exact, ?CALL_MORE, true, true, ?CALL_FOUND, ?CALL_MISSED).
?GET(call_hashed, call_walk_hashed, ?CALL_MORE, ?SAME_KEY, ?SAME_NAME, ?CALL_FOUND,
     ?CALL_MISSED).
?WALK(call_walk_hashed, ?CALL_MORE, ?SAME_KEY, ?SAME_NAME, ?CALL_FOUND, ?CALL_MISSED).

get_missed(js_tdz, St, ?ENV, _, Pc, _) -> ?CORE:tdz(js_tdz, St, ?ENV, Pc);
get_missed(Recv, St, ?ENV, N, Pc, K) ->
    case arc_interp_prop_ffi:find_accessor(A, Recv, K) of
        {accessor, {?SOME, Getter}, _} -> ?CALLS:call_at(Getter, St, ?ENV, N, Pc, [], Recv);
        _ -> ?CORE:via({get_field, K}, [Recv | St], ?ENV, N, Pc, c)
    end.

call_missed(Recv, St, ?ENV, N, Pc1, Line, Mode, Args, K) ->
    Call = fun(Method, [This | St1], F1, L1, A1, C1, Q0, Q1, Q2, Q3) ->
        ?CALLS:call(Method, St1, F1, L1, A1, C1, Q0, Q1, Q2, Q3, N, Pc1, Line, Mode, Args,
                    This)
    end,
    get_missed(Recv, [Recv | St], ?ENV, Call, Pc1 - 1, K).

get_field({_, Id, _} = K, {?SEEN}, N, Pc) when ?IS_OP(N), ?ID_IS_EXACT(Id) ->
    ?FN -> get_exact(T, St, ?ENV, N, Pc, Id, K, ?SEEN) end;
get_field({_, Id, _} = K, {?SEEN}, N, Pc) when ?IS_OP(N) ->
    ?FN -> get_hashed(T, St, ?ENV, N, Pc, Id, K, ?SEEN) end.

get_field_keep({_, Id, _} = K, {?SEEN}, N, Pc) when ?IS_OP(N), ?ID_IS_EXACT(Id) ->
    ?FN -> get_exact(T, [T | St], ?ENV, N, Pc, Id, K, ?SEEN) end;
get_field_keep({_, Id, _} = K, {?SEEN}, N, Pc) when ?IS_OP(N) ->
    ?FN -> get_hashed(T, [T | St], ?ENV, N, Pc, Id, K, ?SEEN) end.

get_local_field(I, {_, Id, _} = K, {?SEEN}, S, N, Pc) when ?IS_OP(N), ?ID_IS_EXACT(Id) ->
    ?PLACED(I, S, X, get_exact(X, St, ?ENV, N, Pc, Id, K, ?SEEN));
get_local_field(I, {_, Id, _} = K, {?SEEN}, S, N, Pc) when ?IS_OP(N) ->
    ?PLACED(I, S, X, get_hashed(X, St, ?ENV, N, Pc, Id, K, ?SEEN)).

get_local_field_keep(I, {_, Id, _} = K, {?SEEN}, S, N, Pc) when ?IS_OP(N), ?ID_IS_EXACT(Id) ->
    ?PLACED(I, S, X, get_exact(X, [X | St], ?ENV, N, Pc, Id, K, ?SEEN));
get_local_field_keep(I, {_, Id, _} = K, {?SEEN}, S, N, Pc) when ?IS_OP(N) ->
    ?PLACED(I, S, X, get_hashed(X, [X | St], ?ENV, N, Pc, Id, K, ?SEEN)).

field_call({_, Id, _} = K, {?SEEN}, N, Pc1, {Line, Mode}) when ?ID_IS_EXACT(Id) ->
    ?FN -> call_exact(T, St, ?ENV, N, Pc1, Line, Mode, [], Id, K, ?SEEN) end;
field_call({_, Id, _} = K, {?SEEN}, N, Pc1, {Line, Mode}) ->
    ?FN -> call_hashed(T, St, ?ENV, N, Pc1, Line, Mode, [], Id, K, ?SEEN) end.

%% the method is read before the argument can throw
field_call1({_, Id, _} = K, {?SEEN}, J, N, Pc1, {Line, Mode}) when ?ID_IS_EXACT(Id) ->
    ?FN ->
        case ?RD(J) of
            js_tdz -> ?CORE:via_flushed([T | St], ?ENV, N, Pc1 - 1, c);
            Arg -> call_exact(T, St, ?ENV, N, Pc1, Line, Mode, [Arg], Id, K, ?SEEN)
        end
    end;
field_call1({_, Id, _} = K, {?SEEN}, J, N, Pc1, {Line, Mode}) ->
    ?FN ->
        case ?RD(J) of
            js_tdz -> ?CORE:via_flushed([T | St], ?ENV, N, Pc1 - 1, c);
            Arg -> call_hashed(T, St, ?ENV, N, Pc1, Line, Mode, [Arg], Id, K, ?SEEN)
        end
    end.

local_field_call(I, {_, Id, _} = K, {?SEEN}, S, N, Pc1, {Line, Mode}) when ?ID_IS_EXACT(Id) ->
    ?PLACED(I, S, X, call_exact(X, St, ?ENV, N, Pc1, Line, Mode, [], Id, K, ?SEEN));
local_field_call(I, {_, Id, _} = K, {?SEEN}, S, N, Pc1, {Line, Mode}) ->
    ?PLACED(I, S, X, call_hashed(X, St, ?ENV, N, Pc1, Line, Mode, [], Id, K, ?SEEN)).

%% §10.1.9.2 ordinary set. The value stays in T for whoever wants it, and Out
%% says whether N does
put_field(K, Hint, N, Pc) when ?IS_OP(N) -> put_top(K, Hint, N, Pc, c).

put_field_pop(K, Hint, N, Pc) when ?IS_OP(N) -> put_top(K, Hint, N, Pc, e).

put_top({_, Id, _} = K, {?SEEN}, N, Pc, Out) when ?ID_IS_EXACT(Id) ->
    fun(T, [Recv | St], ?ENV) -> put_exact(T, St, ?ENV, N, Pc, Out, Id, K, ?SEEN, Recv) end;
put_top({_, Id, _} = K, {?SEEN}, N, Pc, Out) ->
    fun(T, [Recv | St], ?ENV) -> put_hashed(T, St, ?ENV, N, Pc, Out, Id, K, ?SEEN, Recv) end.

-define(UNLESS_TDZ(X, Body),
        case X of
            js_tdz -> ?CORE:tdz(T, St, ?ENV, Pc);
            V -> Body
        end).
-define(PUT_FROM(Put, From, Recv),
        ?OP(?UNLESS_TDZ(From, Put(V, St, ?ENV, N, Pc, e, Id, K, ?SEEN, Recv)))).
%% the object is nearly always this, and the value an early parameter
-define(PUT_LOCAL_LOCAL(Put),
        case {I, J} of
            {1, 2} -> ?PUT_FROM(Put, element(2, L), element(1, L));
            {1, 3} -> ?PUT_FROM(Put, element(3, L), element(1, L));
            {1, 4} -> ?PUT_FROM(Put, element(4, L), element(1, L));
            {1, _} -> ?PUT_FROM(Put, ?RD(J), element(1, L));
            _ -> ?PUT_FROM(Put, ?RD(J), ?RD(I))
        end).

%% built with nothing in T
put_local_field(local, I, J, {_, Id, _} = K, {?SEEN}, N, Pc) when ?IS_OP(N), ?ID_IS_EXACT(Id) ->
    ?PUT_LOCAL_LOCAL(put_exact);
put_local_field(local, I, J, {_, Id, _} = K, {?SEEN}, N, Pc) when ?IS_OP(N) ->
    ?PUT_LOCAL_LOCAL(put_hashed);
put_local_field(const, 1, V, {_, Id, _} = K, {?SEEN}, N, Pc) when ?IS_OP(N), ?ID_IS_EXACT(Id) ->
    ?FN -> put_exact(V, St, ?ENV, N, Pc, e, Id, K, ?SEEN, element(1, L)) end;
put_local_field(const, I, V, {_, Id, _} = K, {?SEEN}, N, Pc) when ?IS_OP(N), ?ID_IS_EXACT(Id) ->
    ?FN -> put_exact(V, St, ?ENV, N, Pc, e, Id, K, ?SEEN, ?RD(I)) end;
put_local_field(const, I, V, {_, Id, _} = K, {?SEEN}, N, Pc) when ?IS_OP(N) ->
    ?FN -> put_hashed(V, St, ?ENV, N, Pc, e, Id, K, ?SEEN, ?RD(I)) end.

-define(FILLING(This, Slots, Unset),
        ?FN ->
            case F#frame.return_to of
                Saved when element(#saved_frame.constructor_this, Saved) =/= ?NONE ->
                    case Slots of
                        Unset -> ?GO(OneByOne, T, St);
                        Filled -> fill(T, St, ?ENV, OneByOne, N, This, Ids, Shape, Offs, Filled)
                    end;
                _ -> ?GO(OneByOne, T, St)
            end
        end).
-define(SET(V), V =/= js_tdz).

%% the writes a constructor opens with, made at once when they find the object
%% as new. built with nothing in T
first_fields(1, Ids, [2, 3], Shape, Offs, OneByOne, N) when ?IS_OP(OneByOne), ?IS_OP(N) ->
    ?FILLING(element(1, L), {element(2, L), element(3, L)},
             {V1, V2} when not (?SET(V1) andalso ?SET(V2)));
first_fields(1, Ids, [2, 3, 4], Shape, Offs, OneByOne, N) when ?IS_OP(OneByOne), ?IS_OP(N) ->
    ?FILLING(element(1, L), {element(2, L), element(3, L), element(4, L)},
             {V1, V2, V3} when not (?SET(V1) andalso ?SET(V2) andalso ?SET(V3)));
first_fields(I, Ids, Sources, Shape, Offs, OneByOne, N) when ?IS_OP(OneByOne), ?IS_OP(N) ->
    ?FILLING(?RD(I), gather(Sources, L, ?RS, []), unset).

gather([{V} | Sources], L, ?RS, Acc) -> gather(Sources, L, ?RS, [V | Acc]);
gather([J | Sources], L, ?RS, Acc) ->
    case ?RD(J) of
        js_tdz -> unset;
        V -> gather(Sources, L, ?RS, [V | Acc])
    end;
gather([], _, _, _, _, _, Acc) -> list_to_tuple(lists:reverse(Acc)).

fill(T, St, ?ENV, OneByOne, N, {?HANDLE_TAG, H}, Ids, Shape, Offs, Slots) ->
    case arena_get(H, ?CELLS(A)) of
        {?SSHAPEDOBJECT_TAG, 0, {?SOME, {?HANDLE_TAG, P}} = Proto, _, _} ->
            case (A#agent.store)#store.plain_write_protos of
                #{P := nil} ->
                    set_cell(T, St, ?ENV, N, H, {?SSHAPEDOBJECT_TAG, Shape, Proto, Slots, Offs});
                #{P := Memo} ->
                    case lists:any(fun(Id) -> is_map_key(Id, Memo) end, Ids) of
                        true -> ?GO(OneByOne, T, St);
                        false ->
                            set_cell(T, St, ?ENV, N, H,
                                     {?SSHAPEDOBJECT_TAG, Shape, Proto, Slots, Offs})
                    end;
                _ -> ?GO(OneByOne, T, St)
            end;
        _ -> ?GO(OneByOne, T, St)
    end;
fill(T, St, ?ENV, OneByOne, _, _, _, _, _, _) -> ?GO(OneByOne, T, St).

-define(PUT(Name, ShapeKey, Step),
Name(V, St, ?ENV, N, Pc, Out, Id, K, ?SEEN, Recv) ->
    case Recv of
        {?HANDLE_TAG, H} ->
            case arena_get(H, ?CELLS(A)) of
                {?SSHAPEDOBJECT_TAG, Seen, Proto, Slots, Offs} ->
                    put_slot(V, St, ?ENV, N, H, Seen, Proto, Slots, Offs, At + 1);
                {?SSHAPEDOBJECT_TAG, Shape, Proto, Slots, Offs} ->
                    case Offs of
                        #{Id := {SK, Off}} when ShapeKey ->
                            put_slot(V, St, ?ENV, N, H, Shape, Proto, Slots, Offs, Off + 1);
                        _ ->
                            add_slot(V, St, ?ENV, N, Pc, Out, Id, K, Recv, Shape, Proto, Slots,
                                     Step)
                    end;
                _ -> put_general(V, St, ?ENV, N, Pc, Out, K, Recv)
            end;
        _ -> put_general(V, St, ?ENV, N, Pc, Out, K, Recv)
    end).

?PUT(put_exact, true, Id).
?PUT(put_hashed, ?SAME_KEY, ?KEY_TEXT(K)).

-define(SIZED(Of, At),
        {Of} ->
            Cell = {?SSHAPEDOBJECT_TAG, Shape, Proto, case D of At end, Offs},
            set_cell(V, St, ?ENV, N, H, Cell)).

put_slot(V, St, ?ENV, N, H, Shape, Proto, Slots, Offs, D) ->
    case Slots of
        ?SIZED(?OF1, ?AT1(Slots, V));
        ?SIZED(?OF2, ?AT2(Slots, V));
        ?SIZED(?OF3, ?AT3(Slots, V));
        ?SIZED(?OF4, ?AT4(Slots, V));
        ?SIZED(?OF5, ?AT5(Slots, V));
        ?SIZED(?OF6, ?AT6(Slots, V));
        ?SIZED(?OF7, ?AT7(Slots, V));
        ?SIZED(?OF8, ?AT8(Slots, V));
        ?SIZED(?OF9, ?AT9(Slots, V));
        ?SIZED(?OF10, ?AT10(Slots, V));
        _ ->
            Cell = {?SSHAPEDOBJECT_TAG, Shape, Proto, setelement(D, Slots, V), Offs},
            set_cell(V, St, ?ENV, N, H, Cell)
    end.

%% along a transition already taken, under a prototype known not to interfere
add_slot(V, St, ?ENV, N, Pc, Out, Id, K, {_, H} = Recv, Shape, Proto, Slots, Step) ->
    case {Proto, A#agent.store} of
        {{?SOME, {?HANDLE_TAG, P}},
         #store{shapes = #{Shape := {?SHAPE_TAG, _, _, #{Step := {To, Offs}}}},
                plain_write_protos = Memos}} ->
            case Memos of
                #{P := Memo} when ?LETS_WRITE(Memo, Id) ->
                    Grown = case Slots of
                        {S1} -> {S1, V};
                        {S1, S2} -> {S1, S2, V};
                        {S1, S2, S3} -> {S1, S2, S3, V};
                        {S1, S2, S3, S4} -> {S1, S2, S3, S4, V};
                        {S1, S2, S3, S4, S5} -> {S1, S2, S3, S4, S5, V};
                        {} -> {V};
                        _ -> erlang:append_element(Slots, V)
                    end,
                    set_cell(V, St, ?ENV, N, H, {?SSHAPEDOBJECT_TAG, To, Proto, Grown, Offs});
                _ -> put_general(V, St, ?ENV, N, Pc, Out, K, Recv)
            end;
        _ -> put_general(V, St, ?ENV, N, Pc, Out, K, Recv)
    end.

put_general(V, St, ?ENV, N, Pc, Out, K, Recv) ->
    case arc_interp_prop_ffi:put_field(A, Recv, K, V, true) of
        miss when Recv =:= js_tdz -> ?CORE:tdz(V, St, ?ENV, Pc);
        miss when Out =:= c -> ?CORE:via({put_field, K}, [V, Recv | St], ?ENV, N, Pc, c);
        miss ->
            %% what a setter returns lands in T, where nothing looks for it
            case arc_interp_prop_ffi:find_accessor(A, Recv, K) of
                {accessor, _, {?SOME, Setter}} ->
                    ?CALLS:call_at(Setter, St, ?ENV, N, Pc, [V], Recv);
                _ -> ?CORE:via({put_field_pop, K}, [V, Recv | St], ?ENV, N, Pc, e)
            end;
        A1 -> ?GO(N, V, St, A1)
    end.

-define(IN_LEAF_SET(Leaf, H, Cell), (case ((H) band ?LEAF_MASK) + 1 of ?AT16(Leaf, Cell) end)).

%% arc_rt_arena_ffi:set/3, for a cell nothing is remembered of
set_cell(T, St, ?ENV, N, H, Cell) ->
    case ?CELLS(A) of
        ?ARENA(H, _, HotIx, Hot, WarmIx, Warm, Low, Shift, Root) ->
            Cells = ?ARENA(H, Cell, HotIx, Hot, WarmIx, Warm, Low, Shift, Root),
            ?GO(N, T, St, A#agent{cells = Cells});
        ?ARENA(LastId, Last, HotIx, {?OF16} = Hot, WarmIx, Warm, Low, Shift, Root)
          when ?IS_CELL_ID(H), ?IS_CELL_ID(LastId), H bsr ?LEAF_BITS =:= HotIx ->
            Cells = ?ARENA(H, Cell, HotIx, ?IN_LEAF_SET(Hot, LastId, Last), WarmIx, Warm, Low,
                           Shift, Root),
            ?GO(N, T, St, A#agent{cells = Cells});
        ?ARENA(LastId, Last, HotIx, {?OF16} = Hot, WarmIx, Warm, Low, Shift, Root)
          when ?IS_CELL_ID(H), ?IS_CELL_ID(LastId), H bsr ?LEAF_BITS =:= WarmIx ->
            Cells = ?ARENA(H, Cell, WarmIx, Warm, HotIx, ?IN_LEAF_SET(Hot, LastId, Last), Low,
                           Shift, Root),
            ?GO(N, T, St, A#agent{cells = Cells});
        Cells -> ?GO(N, T, St, A#agent{cells = arc_rt_arena_ffi:set(H, Cell, Cells)})
    end.

%% pushes a handle to a new cell
new_cell(_, St, ?ENV, N, Cell) when ?IS_OP(N) ->
    case A of
        #agent{cells = ?ARENA(LastId, Last, HotIx, {?OF16} = Hot, WarmIx, Warm, Low, Shift, Root),
               next_id = H}
          when ?IS_CELL_ID(H), ?IS_CELL_ID(LastId), H bsr ?LEAF_BITS =:= HotIx ->
            Cells = ?ARENA(H, Cell, HotIx, ?IN_LEAF_SET(Hot, LastId, Last), WarmIx, Warm, Low,
                           Shift, Root),
            ?GO(N, {?HANDLE_TAG, H}, St, A#agent{cells = Cells, next_id = H + 1});
        #agent{cells = Cells, next_id = H} ->
            ?GO(N, {?HANDLE_TAG, H}, St,
                A#agent{cells = arc_rt_arena_ffi:set(H, Cell, Cells), next_id = H + 1})
    end.

get_elem(N, Pc) when ?IS_OP(N) ->
    ?FN ->
        [Recv | St1] = St,
        elem(T, St1, ?ENV, N, Pc, Recv)
    end.

%% only an integer key is its own canonical key
get_elem_keep(N, Pc) when ?IS_OP(N) ->
    ?FN ->
        case St of
            [Recv | _] when is_integer(T) -> elem(T, [T | St], ?ENV, N, Pc, Recv);
            [Recv | _] when is_binary(T) ->
                case arc_interp_prop_ffi:get_elem(A, Recv, T) of
                    miss -> ?CORE:via(get_elem_keep, [T | St], ?ENV, N, Pc, c);
                    V -> ?GO(N, V, [T | St])
                end;
            _ -> ?CORE:via(get_elem_keep, [T | St], ?ENV, N, Pc, c)
        end
    end.

get_elem_locals(I, J, S, N, Pc) when ?IS_OP(N) ->
    ?PUSHING(S, elem(?RD(J), St, ?ENV, N, Pc, ?RD(I))).

get_elem_post_inc(I, J, S, N, Pc) when ?IS_OP(N) ->
    ?PUSHING(S, begin
        Recv = ?RD(I),
        case ?RD(J) of
            X when is_integer(X), X < ?MAX_SAFE_INT, Recv =/= js_tdz ->
                case J of
                    ?IN_R0 -> elem(X, St, F, L, A, C, X + 1, R1, R2, R3, N, Pc, Recv);
                    ?IN_R1 -> elem(X, St, F, L, A, C, R0, X + 1, R2, R3, N, Pc, Recv);
                    ?IN_R2 -> elem(X, St, F, L, A, C, R0, R1, X + 1, R3, N, Pc, Recv);
                    ?IN_R3 -> elem(X, St, F, L, A, C, R0, R1, R2, X + 1, N, Pc, Recv);
                    _ -> elem(X, St, F, setelement(J, L, X + 1), A, C, ?RS, N, Pc, Recv)
                end;
            _ -> ?CORE:via_flushed(St, ?ENV, N, Pc, c)
        end
    end).

-define(UNLESS_HOLE(Expr),
        case Expr of
            ?ELEMS_HOLE -> elem_general(Key, St, ?ENV, N, Pc, Recv);
            V -> ?GO(N, V, St)
        end).

elem(Key, St, ?ENV, N, Pc, Recv) ->
    case Recv of
        {?HANDLE_TAG, H} when is_integer(Key), Key >= 0, Key =< ?MAX_ARRAY_INDEX ->
            case arena_get(H, ?CELLS(A)) of
                {?SOBJECT_TAG, {?ARRAYOBJ_TAG, Length}, _, Props, _, {?ELEMS_DENSE, Els}, _}
                  when Key < Length, Props =:= #{} ->
                    case Els of
                        {?VEC_TAG, _, _, _, HotIx, Hot} when Key bsr 4 =:= HotIx ->
                            ?UNLESS_HOLE(?IN_NODE(Key, 0, Hot));
                        {?VEC_TAG, Size, Shift, Root, _, _} when Key < Size ->
                            case Shift of
                                4 -> ?UNLESS_HOLE(?IN_NODE(Key, 0, ?IN_NODE(Key, 4, Root)));
                                8 ->
                                    ?UNLESS_HOLE(?IN_NODE(Key, 0, ?IN_NODE(Key, 4,
                                                 ?IN_NODE(Key, 8, Root))));
                                12 ->
                                    ?UNLESS_HOLE(?IN_NODE(Key, 0, ?IN_NODE(Key, 4,
                                                 ?IN_NODE(Key, 8, ?IN_NODE(Key, 12, Root)))));
                                _ -> elem_general(Key, St, ?ENV, N, Pc, Recv)
                            end;
                        {?VEC_TAG, _, _, _, _, _} -> elem_general(Key, St, ?ENV, N, Pc, Recv);
                        %% past the end of the tuple fails the guard
                        _ when element(Key + 1, Els) =/= ?ELEMS_HOLE ->
                            ?GO(N, element(Key + 1, Els), St);
                        _ -> elem_general(Key, St, ?ENV, N, Pc, Recv)
                    end;
                {?SOBJECT_TAG, {?TYPEDARRAYOBJ_TAG, _, _, _, _} = View, _, _, _, _, _} ->
                    typed_elem(Key, St, ?ENV, N, Pc, Recv, View);
                _ -> elem_general(Key, St, ?ENV, N, Pc, Recv)
            end;
        _ -> elem_general(Key, St, ?ENV, N, Pc, Recv)
    end.

typed_elem(Key, St, ?ENV, N, Pc, Recv, {_, {?HANDLE_TAG, Buf}, Kind, Offset, Length}) ->
    {?SOBJECT_TAG, {?ARRAYBUFFEROBJ_TAG, Storage}, _, _, _, _, _} = arena_get(Buf, ?CELLS(A)),
    case arc_rt_buffer_ffi:element_at(Storage, Kind, Offset, Length, Key) of
        miss -> elem_general(Key, St, ?ENV, N, Pc, Recv);
        V -> ?GO(N, V, St)
    end.

elem_general(Key, St, ?ENV, N, _, Recv)
  when is_binary(Recv), is_integer(Key), Key >= 0, Key < byte_size(Recv) ->
    <<_:Key/binary, Code, _/binary>> = Recv,
    ?GO(N, <<Code>>, St);
elem_general(Key, St, ?ENV, N, Pc, Recv) ->
    case arc_interp_prop_ffi:get_elem(A, Recv, Key) of
        miss when Recv =:= js_tdz; Key =:= js_tdz -> ?CORE:tdz(Key, St, ?ENV, Pc);
        miss -> ?CORE:via(get_elem, [Key, Recv | St], ?ENV, N, Pc, c);
        V -> ?GO(N, V, St)
    end.

put_elem(N, Pc) when ?IS_OP(N) ->
    fun(T, [Key, Recv | St], ?ENV) -> put_elem(T, St, ?ENV, N, Pc, c, Key, Recv) end.

put_elem_pop(N, Pc) when ?IS_OP(N) ->
    fun(T, [Key, Recv | St], ?ENV) -> put_elem(T, St, ?ENV, N, Pc, e, Key, Recv) end.

-define(WITH_ELEMENTS(Els),
        set_cell(V, St, ?ENV, N, H,
                 {?SOBJECT_TAG, Kind, Proto, Props, Symbols, {?ELEMS_DENSE, Els}, true})).

%% over a value already there
put_elem(V, St, ?ENV, N, Pc, Out, Key, Recv) ->
    case Recv of
        {?HANDLE_TAG, H} when is_integer(Key), Key >= 0, Key =< ?MAX_ARRAY_INDEX ->
            case arena_get(H, ?CELLS(A)) of
                {?SOBJECT_TAG, {?ARRAYOBJ_TAG, Length} = Kind, Proto, Props, Symbols,
                 {?ELEMS_DENSE, Els}, true}
                  when Key < Length, Props =:= #{} ->
                    case Els of
                        {?VEC_TAG, Size, Shift, Root, HotIx, {?OF16} = Hot}
                          when Key bsr 4 =:= HotIx, ?IN_NODE(Key, 0, Hot) =/= ?ELEMS_HOLE ->
                            Hot1 = ?IN_LEAF_SET(Hot, Key, V),
                            ?WITH_ELEMENTS({?VEC_TAG, Size, Shift, Root, HotIx, Hot1});
                        {?VEC_TAG, _, _, _, _, _} ->
                            case arc_tree_array_ffi:overwrite(Key, V, Els) of
                                hole -> put_elem_general(V, St, ?ENV, N, Pc, Out, Key, Recv);
                                Els1 -> ?WITH_ELEMENTS(Els1)
                            end;
                        _ when element(Key + 1, Els) =/= ?ELEMS_HOLE ->
                            ?WITH_ELEMENTS(setelement(Key + 1, Els, V));
                        _ -> put_elem_general(V, St, ?ENV, N, Pc, Out, Key, Recv)
                    end;
                {?SOBJECT_TAG, {?TYPEDARRAYOBJ_TAG, _, _, _, _} = View, _, _, _, _, _} ->
                    put_typed_elem(V, St, ?ENV, N, Pc, Out, Key, Recv, View);
                _ -> put_elem_general(V, St, ?ENV, N, Pc, Out, Key, Recv)
            end;
        _ -> put_elem_general(V, St, ?ENV, N, Pc, Out, Key, Recv)
    end.

put_typed_elem(V, St, ?ENV, N, Pc, Out, Key, Recv,
               {_, {?HANDLE_TAG, Buf}, Kind, Offset, Length}) ->
    {?SOBJECT_TAG, {?ARRAYBUFFEROBJ_TAG, Storage}, Proto, Props, Symbols, Els, Extensible} =
        arena_get(Buf, ?CELLS(A)),
    case arc_rt_buffer_ffi:with_element(Storage, Kind, Offset, Length, Key, V) of
        miss -> put_elem_general(V, St, ?ENV, N, Pc, Out, Key, Recv);
        Storage1 ->
            set_cell(V, St, ?ENV, N, Buf,
                     {?SOBJECT_TAG, {?ARRAYBUFFEROBJ_TAG, Storage1}, Proto, Props, Symbols, Els,
                      Extensible})
    end.

put_elem_general(V, St, ?ENV, N, Pc, Out, Key, Recv) ->
    case arc_interp_prop_ffi:put_elem(A, Recv, Key, V) of
        miss when Out =:= c -> ?CORE:via(put_elem, [V, Key, Recv | St], ?ENV, N, Pc, c);
        miss -> ?CORE:via(put_elem_pop, [V, Key, Recv | St], ?ENV, N, Pc, e);
        A1 -> ?GO(N, V, St, A1)
    end.

%% §9.1.1.4.6 global getbindingvalue
get_global({?KEY_NAMED, Id, _} = K, S, N, Pc) when ?IS_OP(N), ?ID_IS_EXACT(Id) ->
    ?PUSHING(S, case A#agent.globals of
                    #{Id := {_, V}} -> ?GO(N, V, St);
                    _ -> read_global(T, St, ?ENV, N, Pc, K)
                end);
get_global({?KEY_NAMED, Id, Text} = K, S, N, Pc) when ?IS_OP(N) ->
    ?PUSHING(S, case A#agent.globals of
                    #{Id := {Text, V}} -> ?GO(N, V, St);
                    _ -> read_global(T, St, ?ENV, N, Pc, K)
                end).

%% a binding, or a data property of the global object's own, is worth remembering
read_global(_, St, ?ENV, N, Pc, {_, Id, Text} = K) ->
    #agent{realm = Realm, globals = Globals} = A,
    Lex = element(?REALM_LEXICAL_GLOBALS, Realm),
    {?HANDLE_TAG, G} = element(?REALM_GLOBAL_OBJECT, Realm),
    case Lex of
        #{Id := {Text, {_, V}}} when V =/= js_tdz ->
            ?GO(N, V, St, A#agent{globals = Globals#{Id => {Text, V}}});
        #{Id := _} -> ?CORE:via({get_global, K}, St, ?ENV, N, Pc, c);
        _ ->
            case arc_rt_arena_ffi:get(G, ?CELLS(A)) of
                {?SOBJECT_TAG, ?GLOBALOBJ, _,
                 #{Id := {Text, {?DATAPROPERTY_TAG, V, _, _, _, _}}}, _, _, _} ->
                    ?GO(N, V, St, A#agent{globals = Globals#{Id => {Text, V}}});
                _ ->
                    case arc_interp_prop_ffi:get_global(A, Lex, K) of
                        miss -> ?CORE:via({get_global, K}, St, ?ENV, N, Pc, c);
                        V -> ?GO(N, V, St)
                    end
            end
    end.

typeof_global(K, S, N, Pc) when ?IS_OP(N) ->
    ?PUSHING(S, begin
        Lex = element(?REALM_LEXICAL_GLOBALS, A#agent.realm),
        Type = case arc_interp_prop_ffi:get_global(A, Lex, K) of
            miss -> miss;
            V -> arc_interp_ffi:type_of(A, V)
        end,
        case Type of
            miss -> ?CORE:via({typeof_global, K}, St, ?ENV, N, Pc, c);
            _ -> ?GO(N, Type, St)
        end
    end).

%% §9.1.1.4.5 setmutablebinding; a plain write changes no other name's answer
put_global({_, Id, Text} = K, Strict, N, Pc) when ?IS_OP(N) ->
    ?FN ->
        #agent{realm = Realm, globals = Globals} = A,
        case element(?REALM_LEXICAL_GLOBALS, Realm) of
            #{Id := {Text, {'let', Old}}} = Lex when Old =/= js_tdz ->
                Realm1 = setelement(?REALM_LEXICAL_GLOBALS, Realm,
                                    Lex#{Id := {Text, {'let', T}}}),
                ?GO(N, T, St, A#agent{realm = Realm1, globals = Globals#{Id => {Text, T}}});
            Lex ->
                Global = element(?REALM_GLOBAL_OBJECT, Realm),
                case arc_interp_prop_ffi:put_global(A, Lex, Global, K, T, Strict) of
                    miss -> ?CORE:via({put_global, K}, [T | St], ?ENV, N, Pc, e);
                    A1 -> ?GO(N, T, St, A1#agent{globals = Globals#{Id => {Text, T}}})
                end
        end
    end.

get_boxed(I, S, N, Pc) when ?IS_OP(N) -> ?PLACED(I, S, X, unbox(X, St, ?ENV, N, Pc)).

-define(ENV_BOXED(I), ?PUSHING(S, unbox(element(I, element(1, L)), St, ?ENV, N, Pc))).
get_env_boxed(1, S, N, Pc) when ?IS_OP(N) -> ?ENV_BOXED(1);
get_env_boxed(2, S, N, Pc) when ?IS_OP(N) -> ?ENV_BOXED(2);
get_env_boxed(3, S, N, Pc) when ?IS_OP(N) -> ?ENV_BOXED(3);
get_env_boxed(4, S, N, Pc) when ?IS_OP(N) -> ?ENV_BOXED(4);
get_env_boxed(5, S, N, Pc) when ?IS_OP(N) -> ?ENV_BOXED(5);
get_env_boxed(6, S, N, Pc) when ?IS_OP(N) -> ?ENV_BOXED(6);
get_env_boxed(7, S, N, Pc) when ?IS_OP(N) -> ?ENV_BOXED(7);
get_env_boxed(8, S, N, Pc) when ?IS_OP(N) -> ?ENV_BOXED(8);
get_env_boxed(I, S, N, Pc) when ?IS_OP(N) -> ?ENV_BOXED(I).

unbox(Box, St, ?ENV, N, Pc) ->
    case Box of
        {?HANDLE_TAG, H} ->
            case arena_get(H, ?CELLS(A)) of
                {?SBOX_TAG, V} when V =/= js_tdz -> ?GO(N, V, St);
                _ -> ?CORE:via_op(St, ?ENV, N, Pc, c)
            end;
        _ -> ?CORE:via_op(St, ?ENV, N, Pc, c)
    end.

put_boxed(I, N, Pc) when ?IS_OP(N) -> ?FN -> set_box(T, St, ?ENV, N, Pc, element(I, L)) end.

put_env_boxed(I, N, Pc) when ?IS_OP(N) ->
    ?FN -> set_box(T, St, ?ENV, N, Pc, element(I, element(1, L))) end.

set_box(V, St, ?ENV, N, Pc, Box) ->
    case Box of
        {?HANDLE_TAG, H} -> set_cell(V, St, ?ENV, N, H, {?SBOX_TAG, V});
        _ -> ?CORE:via_op([V | St], ?ENV, N, Pc, e)
    end.

box_local(I, N) ->
    ?FN ->
        #agent{cells = Cells, next_id = Id} = A,
        Cells1 = arc_rt_arena_ffi:set(Id, {?SBOX_TAG, element(I, L)}, Cells),
        ?CORE:put_local(T, St, F, L, A#agent{cells = Cells1, next_id = Id + 1}, C, ?RS, N, I,
                        {?HANDLE_TAG, Id})
    end.
