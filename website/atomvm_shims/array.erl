%% AtomVM shim — minimal subset of OTP's array module.
%%
%% OTP uses a 10-ary leaf-node tree for O(log n) access. Arc's hot path
%% only touches this via tree_array (JS dense elements), so we back it
%% with a map keyed by index. Not as tight as the tree but O(1) average
%% and the code is small.
-module(array).
-export([new/1, from_list/2, to_list/1, get/2, set/3, reset/2,
         size/1, resize/2, default/1, sparse_foldl/3]).

-record(arr, {size = 0, default, map = #{}}).

new({default, D}) ->
    #arr{default = D}.

from_list(L, D) ->
    from_list(L, D, 0, #{}).
from_list([], D, N, M) ->
    #arr{size = N, default = D, map = M};
from_list([H | T], D, N, M) ->
    from_list(T, D, N + 1, M#{N => H}).

to_list(#arr{size = N, default = D, map = M}) ->
    to_list(0, N, D, M, []).
to_list(I, N, _, _, Acc) when I >= N ->
    lists:reverse(Acc);
to_list(I, N, D, M, Acc) ->
    to_list(I + 1, N, D, M, [maps:get(I, M, D) | Acc]).

get(I, #arr{size = N, default = D, map = M}) when I >= 0, I < N ->
    maps:get(I, M, D);
get(I, #arr{default = D}) when I >= 0 ->
    D.

set(I, V, A = #arr{size = N, map = M}) when I >= 0 ->
    A#arr{size = max(N, I + 1), map = M#{I => V}}.

reset(I, A = #arr{map = M}) when I >= 0 ->
    A#arr{map = maps:remove(I, M)}.

size(#arr{size = N}) -> N.

default(#arr{default = D}) -> D.

resize(NewSize, A = #arr{size = Old, map = M}) when NewSize =< Old ->
    A#arr{size = NewSize,
          map = maps:filter(fun(K, _) -> K < NewSize end, M)};
resize(NewSize, A) ->
    A#arr{size = NewSize}.

sparse_foldl(F, Acc, #arr{map = M}) ->
    lists:foldl(fun(K, A) -> F(K, maps:get(K, M), A) end,
                Acc, lists:sort(maps:keys(M))).
