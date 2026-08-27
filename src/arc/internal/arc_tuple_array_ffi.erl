-module(arc_tuple_array_ffi).
-export([array_get/2, array_repeat/2,
         array_get_unchecked/2, array_set_unchecked/3]).

array_get(Index, Tuple) ->
    case Index >= 0 andalso Index < tuple_size(Tuple) of
        true -> {some, element(Index + 1, Tuple)};
        false -> none
    end.

array_get_unchecked(Index, Tuple) ->
    element(Index + 1, Tuple).
array_set_unchecked(Index, Value, Tuple) ->
    setelement(Index + 1, Tuple, Value).

%% sanity cap on one allocation, ~80MB
-define(MAX_TUPLE_ALLOC, 10000000).

array_repeat(Value, Count) when Count =< ?MAX_TUPLE_ALLOC ->
    erlang:make_tuple(Count, Value);
array_repeat(_Value, _Count) ->
    erlang:error(array_too_large).
