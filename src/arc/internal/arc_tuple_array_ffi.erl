-module(arc_tuple_array_ffi).
-export([get/2, repeat/2,
         get_unchecked/2, set_unchecked/3]).

get(Index, Tuple) ->
    case Index >= 0 andalso Index < tuple_size(Tuple) of
        true -> {some, element(Index + 1, Tuple)};
        false -> none
    end.

get_unchecked(Index, Tuple) ->
    element(Index + 1, Tuple).
set_unchecked(Index, Value, Tuple) ->
    setelement(Index + 1, Tuple, Value).

%% sanity cap on one allocation, ~80MB
-define(MAX_TUPLE_ALLOC, 10000000).

repeat(Value, Count) when Count =< ?MAX_TUPLE_ALLOC ->
    erlang:make_tuple(Count, Value);
repeat(_Value, _Count) ->
    erlang:error(array_too_large).
