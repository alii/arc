-module(arc_rt_gc_ffi).
-export([refs_in_term/2, refs_in_props/2, refs_in_symbol_props/2,
         diff_refs/3]).

-include("arc_rt_layout.hrl").

%% walks fun env too so closures keep captured handles alive
refs_in_term({js_cell, N}, Acc) when is_integer(N) -> [N | Acc];
refs_in_term(F, Acc) when is_function(F) ->
    {env, Env} = erlang:fun_info(F, env),
    lists:foldl(fun refs_in_term/2, Acc, Env);
refs_in_term(T, Acc) when is_tuple(T) -> refs_in_tuple(T, tuple_size(T), Acc);
refs_in_term([H | T], Acc) -> refs_in_term(T, refs_in_term(H, Acc));
refs_in_term(M, Acc) when is_map(M) ->
    maps:fold(fun(K, V, A) -> refs_in_term(V, refs_in_term(K, A)) end, Acc, M);
refs_in_term(_, Acc) -> Acc.

refs_in_tuple(_, 0, Acc) -> Acc;
refs_in_tuple(T, I, Acc) -> refs_in_tuple(T, I - 1, refs_in_term(element(I, T), Acc)).

refs_in_props(Props, Acc) -> refs_in_prop_list(maps:values(Props), Acc).

refs_in_symbol_props([{_, P} | T], Acc) -> refs_in_symbol_props(T, refs_in_prop(P, Acc));
refs_in_symbol_props([], Acc) -> Acc.

refs_in_prop_list([P | T], Acc) -> refs_in_prop_list(T, refs_in_prop(P, Acc));
refs_in_prop_list([], Acc) -> Acc.

refs_in_prop({?DATAPROP_TAG, V, _, _, _, _}, Acc) -> refs_in_term(V, Acc);
refs_in_prop({?ACCESSORPROP_TAG, G, S, _, _, _}, Acc) -> refs_in_term(G, refs_in_term(S, Acc));
refs_in_prop(P, Acc) -> refs_in_term(P, Acc).

%% refs in the parts of New that differ from Old; what Old already held can
%% only name cells at least as old as Old itself, and =:= is cheap on shared parts
diff_refs(Old, New, Acc) ->
    case Old =:= New of
        true -> Acc;
        false -> diff1(Old, New, Acc)
    end.

diff1(_, {js_cell, N}, Acc) when is_integer(N) -> [N | Acc];
diff1(Old, New, Acc) when is_tuple(Old), is_tuple(New),
                          tuple_size(Old) =:= tuple_size(New) ->
    diff_tuple(Old, New, tuple_size(New), Acc);
diff1([OH | OT], [NH | NT], Acc) -> diff_refs(OT, NT, diff_refs(OH, NH, Acc));
diff1(Old, New, Acc) when is_map(Old), is_map(New) ->
    maps:fold(fun(K, V, A) ->
                  case Old of
                      #{K := OV} -> diff_refs(OV, V, A);
                      _ -> refs_in_term(V, refs_in_term(K, A))
                  end
              end, Acc, New);
diff1(_, New, Acc) -> refs_in_term(New, Acc).

diff_tuple(_, _, 0, Acc) -> Acc;
diff_tuple(O, N, I, Acc) ->
    diff_tuple(O, N, I - 1, diff_refs(element(I, O), element(I, N), Acc)).
