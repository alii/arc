%% Superset of AtomVM's estdlib string.erl plus the OTP-25+ functions
%% gleam_stdlib.erl calls. Everything operates on binaries since that's all
%% Gleam passes in. ASCII-only case folding — good enough for the playground.
-module(string).
-export([to_upper/1, to_lower/1, uppercase/1, lowercase/1,
         split/2, split/3, trim/1, trim/2, trim/3, find/2, find/3,
         length/1, is_empty/1, equal/2, reverse/1,
         slice/2, slice/3, pad/4, replace/4, next_grapheme/1]).

to_upper(S) when is_list(S) -> [upper(C) || C <- S];
to_upper(C) when is_integer(C) -> upper(C).
to_lower(S) when is_list(S) -> [lower(C) || C <- S];
to_lower(C) when is_integer(C) -> lower(C).

upper(C) when C >= $a, C =< $z -> C - 32;
upper(C) -> C.
lower(C) when C >= $A, C =< $Z -> C + 32;
lower(C) -> C.

uppercase(B) when is_binary(B) -> << <<(upper(C))/utf8>> || <<C/utf8>> <= B >>;
uppercase(L) -> to_upper(L).
lowercase(B) when is_binary(B) -> << <<(lower(C))/utf8>> || <<C/utf8>> <= B >>;
lowercase(L) -> to_lower(L).

split(S, P) -> split(S, P, leading).
%% Charlists (arc's number formatting hands float_to_list output through here).
split(L, P, Where) when is_list(L) -> split_list(L, to_list(P), Where);
split(S, P, leading) when is_binary(S) -> binary:split(S, to_bin(P));
split(S, P, all) when is_binary(S) -> binary:split(S, to_bin(P), [global]);
split(S, P, trailing) when is_binary(S) ->
    case binary:matches(S, to_bin(P)) of
        [] -> [S];
        Ms -> {Pos, Len} = lists:last(Ms),
              [binary:part(S, 0, Pos), binary:part(S, Pos + Len, byte_size(S) - Pos - Len)]
    end.

trim(S) -> trim(S, both).
trim(L, Dir) when is_list(L) -> trim(L, Dir, " \t\n\r");
trim(B, leading) when is_binary(B) -> ltrim(B);
trim(B, trailing) when is_binary(B) -> rtrim(B);
trim(B, both) when is_binary(B) -> rtrim(ltrim(B)).

%% trim/3: strip any of Chars from the given end(s). Lists directly; binaries
%% via a list round-trip (Chars here are always ASCII in practice).
trim(L, Dir, Chars) when is_list(L) -> trim_list(L, Dir, to_list(Chars));
trim(B, Dir, Chars) when is_binary(B) ->
    unicode:characters_to_binary(trim_list(unicode:characters_to_list(B), Dir, to_list(Chars))).

trim_list(L, leading, Cs) -> lists:dropwhile(fun(C) -> lists:member(C, Cs) end, L);
trim_list(L, trailing, Cs) -> lists:reverse(trim_list(lists:reverse(L), leading, Cs));
trim_list(L, both, Cs) -> trim_list(trim_list(L, leading, Cs), trailing, Cs).

%% split_list(List, PatternList, leading | trailing | all)
split_list(L, P, leading) ->
    case find_sub(L, P, 0) of
        nomatch -> [L];
        Pos -> [lists:sublist(L, Pos), lists:nthtail(Pos + erlang:length(P), L)]
    end;
split_list(L, P, all) ->
    case find_sub(L, P, 0) of
        nomatch -> [L];
        Pos -> [lists:sublist(L, Pos) | split_list(lists:nthtail(Pos + erlang:length(P), L), P, all)]
    end;
split_list(L, P, trailing) ->
    case find_sub_last(L, P, 0, nomatch) of
        nomatch -> [L];
        Pos -> [lists:sublist(L, Pos), lists:nthtail(Pos + erlang:length(P), L)]
    end.

find_sub([], _P, _I) -> nomatch;
find_sub(L, P, I) ->
    case is_prefix(P, L) of
        true -> I;
        false -> find_sub(tl(L), P, I + 1)
    end.

find_sub_last([], _P, _I, Last) -> Last;
find_sub_last(L, P, I, Last) ->
    Next = case is_prefix(P, L) of true -> I; false -> Last end,
    find_sub_last(tl(L), P, I + 1, Next).

%% AtomVM's lists has no prefix/2.
is_prefix([], _) -> true;
is_prefix([C | Ps], [C | Ls]) -> is_prefix(Ps, Ls);
is_prefix(_, _) -> false.

to_list(L) when is_list(L) -> L;
to_list(B) when is_binary(B) -> unicode:characters_to_list(B).

ltrim(<<C, R/binary>>) when C =:= $\s; C =:= $\t; C =:= $\n; C =:= $\r -> ltrim(R);
ltrim(B) -> B.
rtrim(<<>>) -> <<>>;
rtrim(B) ->
    case binary:last(B) of
        C when C =:= $\s; C =:= $\t; C =:= $\n; C =:= $\r ->
            rtrim(binary:part(B, 0, byte_size(B) - 1));
        _ -> B
    end.

find(S, P) -> find(S, P, leading).
find(B, P, leading) when is_binary(B) ->
    case binary:match(B, to_bin(P)) of
        nomatch -> nomatch;
        {Pos, _} -> binary:part(B, Pos, byte_size(B) - Pos)
    end;
find(B, P, trailing) when is_binary(B) ->
    case binary:matches(B, to_bin(P)) of
        [] -> nomatch;
        Ms -> {Pos, _} = lists:last(Ms),
              binary:part(B, Pos, byte_size(B) - Pos)
    end.

length(B) when is_binary(B) -> cp_length(B, 0);
length(L) when is_list(L) -> erlang:length(L).
cp_length(<<>>, N) -> N;
cp_length(<<_/utf8, R/binary>>, N) -> cp_length(R, N + 1);
cp_length(<<_, R/binary>>, N) -> cp_length(R, N + 1).

is_empty(<<>>) -> true;
is_empty([]) -> true;
is_empty(_) -> false.

equal(A, B) -> A =:= B.

reverse(B) when is_binary(B) ->
    rev(B, <<>>);
reverse(L) -> lists:reverse(L).
rev(<<>>, Acc) -> Acc;
rev(<<C/utf8, R/binary>>, Acc) -> rev(R, <<C/utf8, Acc/binary>>);
rev(<<C, R/binary>>, Acc) -> rev(R, <<C, Acc/binary>>).

slice(S, Start) -> slice(S, Start, infinity).
slice(B, Start, Len) when is_binary(B) ->
    drop(B, Start, Len);
slice(L, Start, Len) when is_list(L) ->
    Flat = lists:flatten(L),
    case Start >= erlang:length(Flat) of
        true -> [];
        false ->
            Rest = lists:nthtail(Start, Flat),
            case Len of
                infinity -> Rest;
                _ -> lists:sublist(Rest, Len)
            end
    end.
drop(B, 0, infinity) -> B;
drop(B, 0, Len) -> take(B, Len, <<>>);
drop(<<>>, _, _) -> <<>>;
drop(<<_/utf8, R/binary>>, N, Len) -> drop(R, N - 1, Len);
drop(<<_, R/binary>>, N, Len) -> drop(R, N - 1, Len).
take(_, 0, Acc) -> Acc;
take(<<>>, _, Acc) -> Acc;
take(<<C/utf8, R/binary>>, N, Acc) -> take(R, N - 1, <<Acc/binary, C/utf8>>);
take(<<C, R/binary>>, N, Acc) -> take(R, N - 1, <<Acc/binary, C>>).

pad(B, Len, Dir, [Char]) when is_binary(B) ->
    Cur = cp_length(B, 0),
    Fill = max(0, Len - Cur),
    P = << <<Char/utf8>> || _ <- lists:seq(1, Fill) >>,
    case Dir of
        leading -> [P, B];
        trailing -> [B, P]
    end.

replace(B, Pat, Rep, all) when is_binary(B) ->
    join(binary:split(B, to_bin(Pat), [global]), to_bin(Rep)).

join([], _) -> <<>>;
join([H], _) -> H;
join([H | T], Sep) -> lists:foldl(fun(E, A) -> <<A/binary, Sep/binary, E/binary>> end, H, T).

next_grapheme(<<>>) -> [];
next_grapheme(<<C/utf8, R/binary>>) -> [C | R];
next_grapheme(<<C, R/binary>>) -> [C | R].

to_bin(B) when is_binary(B) -> B;
to_bin(L) when is_list(L) -> list_to_binary(L).
