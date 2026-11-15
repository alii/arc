-module(arc_rt_gc_ffi).
-export([push_refs/2, push_props_refs/2, push_symbol_props_refs/2,
         diff_refs/3, diffed/1, mark/4, is_marked/2]).

-include("arc_rt_layout.hrl").
-include("arc_rt_arena.hrl").

%% walks fun env too so closures keep captured handles alive
push_refs({?HANDLE_TAG, N}, Acc) when is_integer(N) -> [N | Acc];
push_refs(F, Acc) when is_function(F) ->
    {env, Env} = erlang:fun_info(F, env),
    lists:foldl(fun push_refs/2, Acc, Env);
%% compiled code holds primitives only
push_refs(T, Acc) when tuple_size(T) =:= ?FUNCTEMPLATE_SIZE,
                       element(1, T) =:= ?FUNCTEMPLATE_TAG ->
    Acc;
push_refs(T, Acc) when is_tuple(T) -> push_tuple_refs(T, tuple_size(T), Acc);
push_refs([H | T], Acc) -> push_refs(T, push_refs(H, Acc));
push_refs(M, Acc) when is_map(M) ->
    maps:fold(fun(K, V, A) -> push_refs(V, push_refs(K, A)) end, Acc, M);
push_refs(_, Acc) -> Acc.

push_tuple_refs(_, 0, Acc) -> Acc;
push_tuple_refs(T, I, Acc) -> push_tuple_refs(T, I - 1, push_refs(element(I, T), Acc)).

push_props_refs(Props, Acc) -> push_prop_list_refs(maps:values(Props), Acc).

push_symbol_props_refs([{_, P} | T], Acc) -> push_symbol_props_refs(T, push_prop_refs(P, Acc));
push_symbol_props_refs([], Acc) -> Acc.

push_prop_list_refs([Entry | T], Acc) ->
    push_prop_list_refs(T, push_prop_refs(element(2, Entry), Acc));
push_prop_list_refs([], Acc) -> Acc.

push_prop_refs({?DATAPROPERTY_TAG, V, _, _, _, _}, Acc) -> val(V, 0, Acc);
push_prop_refs({?ACCESSORPROPERTY_TAG, G, S, _, _, _}, Acc) -> push_refs(G, push_refs(S, Acc)).

%% refs in the parts of New that differ from Old; =:= is cheap on shared parts.
%% A negative number among them, which is no cell, is how much it went through
%% to find them
diff_refs(Old, New, Acc) ->
    case Old =:= New of
        true -> Acc;
        false -> diff_changed(Old, New, Acc)
    end.

diff_changed(_, {?HANDLE_TAG, N}, Acc) when is_integer(N) -> [N | Acc];
diff_changed(_, New, Acc) when tuple_size(New) =:= ?FUNCTEMPLATE_SIZE,
                               element(1, New) =:= ?FUNCTEMPLATE_TAG ->
    Acc;
%% the order has only keys the entries have too
diff_changed({ordered_entries, Old, _, _}, {ordered_entries, New, _, _}, Acc) ->
    diff_refs(Old, New, Acc);
diff_changed(Old, New, Acc) when is_tuple(Old), is_tuple(New),
                          tuple_size(Old) =:= tuple_size(New) ->
    diff_tuple(Old, New, tuple_size(New), Acc);
%% an improper list has no length, and is left to the last clause
diff_changed([_ | _] = Old, [_ | _] = New, Acc) when length(Old) > 0, length(New) > 0 ->
    Length = length(New),
    diff_list(Old, New, Length - length(Old), Length, Acc);
diff_changed(Old, New, Acc) when is_map(Old), is_map(New) ->
    maps:fold(fun(K, V, A) ->
                  case Old of
                      #{K := OV} -> diff_refs(OV, V, A);
                      _ -> push_refs(V, push_refs(K, A))
                  end
              end, [-map_size(New) - 1 | Acc], New);
diff_changed(_, New, Acc) -> push_refs(New, Acc).

%% a list mostly grows or shrinks at the front, and then is only measured, which
%% is far quicker than going through it
diff_list(Old, New, Added, Length, Acc) ->
    Shared = if Added > 0 -> lists:nthtail(Added, New) =:= Old;
                Added < 0 -> lists:nthtail(-Added, Old) =:= New;
                true -> false
             end,
    case Shared of
        true -> push_first(New, Added, [-(Length bsr 3) - 1 | Acc]);
        false -> diff_pairs(Old, New, [-Length | Acc])
    end.

push_first([H | T], Count, Acc) when Count > 0 -> push_first(T, Count - 1, push_refs(H, Acc));
push_first(_, _, Acc) -> Acc.

diff_pairs([OH | OT], [NH | NT], Acc) -> diff_pairs(OT, NT, diff_refs(OH, NH, Acc));
diff_pairs(_, New, Acc) -> push_refs(New, Acc).

diffed(Roots) -> diffed(Roots, 0).

diffed([N | Roots], Sum) when N < 0 -> diffed(Roots, Sum - N);
diffed([_ | Roots], Sum) -> diffed(Roots, Sum);
diffed([], Sum) -> Sum.

diff_tuple(_, _, 0, Acc) -> Acc;
diff_tuple(O, N, I, Acc) ->
    diff_tuple(O, N, I - 1, diff_refs(element(I, O), element(I, N), Acc)).

%% marks what Roots reach among the ids from W up, as {Marks, their weight, the weak
%% containers among them onto Weak, one past the highest}. Marks holds a word of
%% bits for each run of ids that has any; cells made together point at each
%% other, so the word last written rides along outside the map
mark(Cells, Roots, W, Weak) -> mark(Roots, Cells, W, -1, 0, #{}, 0, Weak).

mark([Id | Rest], Cells, W, Ix, Bits, Marks, N, Weak) when ?IS_CELL_ID(Id), Id >= W ->
    Bit = 1 bsl (Id band ?MARK_MASK),
    case Id bsr ?MARK_BITS of
        Ix when Bits band Bit =/= 0 -> mark(Rest, Cells, W, Ix, Bits, Marks, N, Weak);
        Ix -> visit(Id, Rest, Cells, W, Ix, Bits bor Bit, Marks, N, Weak);
        Ix1 ->
            case Marks of
                #{Ix1 := Bits1} when Bits1 band Bit =/= 0 ->
                    mark(Rest, Cells, W, Ix, Bits, Marks, N, Weak);
                #{Ix1 := Bits1} ->
                    visit(Id, Rest, Cells, W, Ix1, Bits1 bor Bit, Marks#{Ix => Bits}, N, Weak);
                _ -> visit(Id, Rest, Cells, W, Ix1, Bit, Marks#{Ix => Bits}, N, Weak)
            end
    end;
mark([_ | Rest], Cells, W, Ix, Bits, Marks, N, Weak) ->
    mark(Rest, Cells, W, Ix, Bits, Marks, N, Weak);
mark([], _, W, Ix, Bits, Marks, N, Weak) ->
    Marks1 = maps:remove(-1, Marks#{Ix => Bits}),
    {Marks1, N, Weak, max(W, past_highest(Marks1))}.

visit(Id, Rest, Cells, W, Ix, Bits, Marks, N, Weak) ->
    case arc_rt_arena_ffi:probe(Id, Cells) of
        {?SSHAPEDOBJECT_TAG, _, Proto, Slots, _} ->
            mark(slots(Slots, W, proto(Proto, W, Rest)), Cells, W, Ix, Bits, Marks, N + 1, Weak);
        {?SBOX_TAG, V} -> mark(val(V, W, Rest), Cells, W, Ix, Bits, Marks, N + 1, Weak);
        {?SOBJECT_TAG, {?ARRAYOBJ_TAG, _}, Proto, Props, [], {?ELEMS_DENSE, Els}, _} = Cell
          when Props =:= #{} ->
            mark(dense(Els, W, proto(Proto, W, Rest)), Cells, W, Ix, Bits, Marks,
                 N + weight(Cell), Weak);
        {?SOBJECT_TAG, ?ORDINARY, Proto, Props, [], ?ELEMS_NONE, _} ->
            Rest1 = push_props_refs(Props, proto(Proto, W, Rest)),
            mark(Rest1, Cells, W, Ix, Bits, Marks, N + 1, Weak);
        {?SOBJECT_TAG, {?BYTECODEFN_TAG, _, Env, Home, _, ?NONE, _, _, {_, Parent}}, Proto, Props,
         [], ?ELEMS_NONE, _}
          when Props =:= #{} ->
            Rest1 = slots(Env, W, proto(Home, W, proto(Parent, W, proto(Proto, W, Rest)))),
            mark(Rest1, Cells, W, Ix, Bits, Marks, N + 1, Weak);
        {?SOBJECT_TAG, {?PROMISEOBJ_TAG, Data}, Proto, Props, [], ?ELEMS_NONE, _}
          when Props =:= #{} ->
            mark(val(Data, W, proto(Proto, W, Rest)), Cells, W, Ix, Bits, Marks, N + 1, Weak);
        ?STORE_FREE_CELL -> mark(Rest, Cells, W, Ix, Bits, Marks, N + 1, Weak);
        Cell ->
            Weak1 = case 'arc@rt@gc':is_weak_cell(Cell) of
                true -> [Id | Weak];
                false -> Weak
            end,
            Rest1 = 'arc@rt@gc':push_cell_refs(Cell, Rest),
            mark(Rest1, Cells, W, Ix, Bits, Marks, N + weight(Cell), Weak1)
    end.

%% in cells, as arc_rt_store_ffi counts them when they are made
weight({?SOBJECT_TAG, {Tag, Table}, _, _, _, _, _}) when Tag =:= ?MAPOBJ_TAG; Tag =:= ?SETOBJ_TAG ->
    1 + (map_size(element(?ORDERED_ENTRIES, Table)) bsr (?ELEMENTS_PER_CELL_BITS - 2));
weight({?SOBJECT_TAG, _, _, _, _, {?ELEMS_DENSE, {?VEC_TAG, Size, _, _, _, _}}, _}) ->
    1 + (Size bsr ?ELEMENTS_PER_CELL_BITS);
weight(_) -> 1.

proto({?SOME, {?HANDLE_TAG, Id}}, W, Acc) when Id >= W -> [Id | Acc];
proto(_, _, Acc) -> Acc.

-compile({inline, [proto/3, val/3, weight/1]}).
val({?HANDLE_TAG, Id}, W, Acc) when Id >= W -> [Id | Acc];
val({?HANDLE_TAG, _}, _, Acc) -> Acc;
val({?STR_TAG, _, _, _}, _, Acc) -> Acc;
val(V, _, Acc) when is_number(V); is_atom(V); is_binary(V) -> Acc;
val(V, _, Acc) -> push_refs(V, Acc).

slots({}, _, Acc) -> Acc;
slots({A}, W, Acc) -> val(A, W, Acc);
slots({A, B}, W, Acc) -> val(A, W, val(B, W, Acc));
slots({A, B, C}, W, Acc) -> val(A, W, val(B, W, val(C, W, Acc)));
slots({A, B, C, D}, W, Acc) -> val(A, W, val(B, W, val(C, W, val(D, W, Acc))));
slots(Slots, W, Acc) -> vals(tuple_to_list(Slots), W, Acc).

vals([V | Vs], W, Acc) -> vals(Vs, W, val(V, W, Acc));
vals([], _, Acc) -> Acc.

dense({?VEC_TAG, _, _, _, _, _} = Vec, W, Acc) -> vals(arc_tree_array_ffi:to_list(Vec), W, Acc);
dense(Flat, W, Acc) -> slots(Flat, W, Acc).

past_highest(Marks) when map_size(Marks) =:= 0 -> 0;
past_highest(Marks) ->
    Ix = lists:max(maps:keys(Marks)),
    (Ix bsl ?MARK_BITS) + width(map_get(Ix, Marks), 0).

width(0, N) -> N;
width(Bits, N) -> width(Bits bsr 1, N + 1).

is_marked(Id, Marks) ->
    Ix = Id bsr ?MARK_BITS,
    case Marks of
        #{Ix := Bits} -> Bits band (1 bsl (Id band ?MARK_MASK)) =/= 0;
        _ -> false
    end.
