%% string methods on an ascii receiver, where a byte is a character. misses
%% unless the arguments need no converting
-module(arc_rt_string_ffi).
-export([ascii_method/3]).

ascii_method(string_prototype_char_at, S, [I]) when is_integer(I), I >= 0 ->
    case S of
        <<_:I/binary, C, _/binary>> -> <<C>>;
        _ -> <<>>
    end;
ascii_method(string_prototype_code_point_at, S, [I]) when is_integer(I), I >= 0 ->
    case S of
        <<_:I/binary, C, _/binary>> -> C;
        _ -> undefined
    end;
ascii_method(string_prototype_slice, S, [From]) when is_integer(From) ->
    between(S, relative(From, byte_size(S)), byte_size(S));
ascii_method(string_prototype_slice, S, [From, To]) when is_integer(From), is_integer(To) ->
    between(S, relative(From, byte_size(S)), relative(To, byte_size(S)));
ascii_method(string_prototype_substring, S, [From]) when is_integer(From) ->
    between(S, clamped(From, byte_size(S)), byte_size(S));
ascii_method(string_prototype_substring, S, [From, To]) when is_integer(From), is_integer(To) ->
    A = clamped(From, byte_size(S)),
    B = clamped(To, byte_size(S)),
    between(S, min(A, B), max(A, B));
ascii_method(string_prototype_substr, S, [From, Count])
  when is_integer(From), is_integer(Count) ->
    Start = relative(From, byte_size(S)),
    between(S, Start, min(Start + Count, byte_size(S)));
ascii_method(string_prototype_index_of, S, [Search]) when is_binary(Search) ->
    index_of(S, Search, 0);
ascii_method(string_prototype_index_of, S, [Search, From])
  when is_binary(Search), is_integer(From) ->
    index_of(S, Search, clamped(From, byte_size(S)));
ascii_method(string_prototype_includes, S, [Search]) when is_binary(Search) ->
    index_of(S, Search, 0) >= 0;
ascii_method(string_prototype_starts_with, S, [Search]) when is_binary(Search) ->
    is_at(S, Search, 0);
ascii_method(string_prototype_starts_with, S, [Search, From])
  when is_binary(Search), is_integer(From) ->
    is_at(S, Search, clamped(From, byte_size(S)));
ascii_method(string_prototype_ends_with, S, [Search]) when is_binary(Search) ->
    is_at(S, Search, byte_size(S) - byte_size(Search));
ascii_method(string_prototype_trim, S, []) -> arc_rt_utf8_ffi:trim_js_ws(S);
ascii_method(string_prototype_to_lower_case, S, []) -> element(2, arc_rt_utf8_ffi:ascii_lower(S));
ascii_method(string_prototype_to_upper_case, S, []) -> element(2, arc_rt_utf8_ffi:ascii_upper(S));
ascii_method(string_prototype_to_string, S, _) -> S;
ascii_method(string_prototype_value_of, S, _) -> S;
ascii_method(_, _, _) -> miss.

relative(I, Size) when I < 0 -> max(Size + I, 0);
relative(I, Size) -> min(I, Size).

clamped(I, _) when I < 0 -> 0;
clamped(I, Size) -> min(I, Size).

between(S, From, To) when From < To -> binary_part(S, From, To - From);
between(_, _, _) -> <<>>.

index_of(_, <<>>, From) -> From;
index_of(S, Search, From) ->
    case arc_rt_utf8_ffi:find(S, Search, From) of
        nomatch -> -1;
        At -> At
    end.

is_at(_, _, At) when At < 0 -> false;
is_at(S, Search, At) ->
    Size = byte_size(Search),
    case S of
        <<_:At/binary, Search:Size/binary, _/binary>> -> true;
        _ -> false
    end.
