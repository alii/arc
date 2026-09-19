-module(arc_rt_gc_ffi).
-export([push_refs/2, push_props_refs/2, push_symbol_props_refs/2,
         diff_refs/3]).

-include("arc_rt_layout.hrl").

%% walks fun env too so closures keep captured handles alive
push_refs({?HANDLE_TAG, N}, Acc) when is_integer(N) -> [N | Acc];
push_refs(F, Acc) when is_function(F) ->
    {env, Env} = erlang:fun_info(F, env),
    lists:foldl(fun push_refs/2, Acc, Env);
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

push_prop_list_refs([P | T], Acc) -> push_prop_list_refs(T, push_prop_refs(P, Acc));
push_prop_list_refs([], Acc) -> Acc.

push_prop_refs({?DATAPROPERTY_TAG, V, _, _, _, _}, Acc) -> push_refs(V, Acc);
push_prop_refs({?ACCESSORPROPERTY_TAG, G, S, _, _, _}, Acc) -> push_refs(G, push_refs(S, Acc)).

%% refs in the parts of New that differ from Old; =:= is cheap on shared parts
diff_refs(Old, New, Acc) ->
    case Old =:= New of
        true -> Acc;
        false -> diff_changed(Old, New, Acc)
    end.

diff_changed(_, {?HANDLE_TAG, N}, Acc) when is_integer(N) -> [N | Acc];
diff_changed(Old, New, Acc) when is_tuple(Old), is_tuple(New),
                          tuple_size(Old) =:= tuple_size(New) ->
    diff_tuple(Old, New, tuple_size(New), Acc);
diff_changed([OH | OT], [NH | NT], Acc) -> diff_refs(OT, NT, diff_refs(OH, NH, Acc));
diff_changed(Old, New, Acc) when is_map(Old), is_map(New) ->
    maps:fold(fun(K, V, A) ->
                  case Old of
                      #{K := OV} -> diff_refs(OV, V, A);
                      _ -> push_refs(V, push_refs(K, A))
                  end
              end, Acc, New);
diff_changed(_, New, Acc) -> push_refs(New, Acc).

diff_tuple(_, _, 0, Acc) -> Acc;
diff_tuple(O, N, I, Acc) ->
    diff_tuple(O, N, I - 1, diff_refs(element(I, O), element(I, N), Acc)).
