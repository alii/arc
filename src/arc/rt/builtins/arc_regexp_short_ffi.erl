%% a backtracking matcher for runs too short to make up for what setting pcre
%% going costs. It takes only patterns on which js and pcre cannot differ: no
%% backreferences or lookbehind, no capture that can match more than once, no
%% repeat of something that can match nothing. A run gives up, for pcre to do
%% over, on meeting a byte past ascii, the end of what it was willing to look
%% at, or too much backtracking
-module(arc_regexp_short_ffi).
-export([compile/2, run/5]).

%% how many bytes a run looks at, and how often it may go back
-define(GLANCE, 32).
-define(WINDOW, 192).
-define(FUEL, 400).

-define(ALL, 16#FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF).
-define(BIT(C), (1 bsl (C))).
%% in a set that may take something past ascii
-define(BEYOND, ?BIT(128)).
-define(SPAN(Lo, Hi), (((1 bsl ((Hi) - (Lo) + 1)) - 1) bsl (Lo))).
-define(DIGITS, ?SPAN($0, $9)).
-define(UPPER, ?SPAN($A, $Z)).
-define(LOWER, ?SPAN($a, $z)).
-define(WORD, (?DIGITS bor ?UPPER bor ?LOWER bor ?BIT($_))).
-define(SPACE, (?SPAN($\t, $\r) bor ?BIT($\s))).
-define(BREAKS, (?BIT($\n) bor ?BIT($\r))).

-record(flags, {caseless, multiline, dot_all}).

%% {Anchored, matcher, what a match can start with}, or none
compile(Pattern, _) when byte_size(Pattern) > 2048 -> none;
compile(Pattern, Flags) ->
    Has = fun(F) -> binary:match(Flags, F) =/= nomatch end,
    Fl = #flags{caseless = Has(<<"i">>), multiline = Has(<<"m">>), dot_all = Has(<<"s">>)},
    try
        Has(<<"v">>) andalso throw(unsupported),
        {Alt, <<>>, _} = alt(Pattern, Fl, 1),
        Done = fun(_, P, _, Caps, _, _) -> [P | Caps] end,
        {anchored(Alt), node(Alt, Done, any), words(first(Alt, any))}
    catch
        throw:unsupported -> none;
        error:{badmatch, _} -> none
    end.

%% parsing. N is the number the next capture gets

alt(Bin, Fl, N) ->
    case seq(Bin, Fl, N, []) of
        {Seq, <<$|, Rest/binary>>, N1} ->
            {{alt, Seqs}, Rest1, N2} = alt(Rest, Fl, N1),
            {{alt, [Seq | Seqs]}, Rest1, N2};
        {Seq, Rest, N1} -> {{alt, [Seq]}, Rest, N1}
    end.

seq(<<>>, _, N, Acc) -> {lists:reverse(Acc), <<>>, N};
seq(<<C, _/binary>> = Bin, _, N, Acc) when C =:= $|; C =:= $) -> {lists:reverse(Acc), Bin, N};
seq(Bin, Fl, N, Acc) ->
    {Atom, Rest, N1} = atom(Bin, Fl, N),
    {Term, Rest1} = quantified(Atom, Rest),
    seq(Rest1, Fl, N1, [Term | Acc]).

atom(<<$^, R/binary>>, #flags{multiline = true}, N) -> {line_start, R, N};
atom(<<$^, R/binary>>, _, N) -> {text_start, R, N};
atom(<<$$, R/binary>>, #flags{multiline = true}, N) -> {line_end, R, N};
atom(<<$$, R/binary>>, _, N) -> {text_end, R, N};
atom(<<$., R/binary>>, #flags{dot_all = true}, N) -> {{set, ?ALL}, R, N};
atom(<<$., R/binary>>, _, N) -> {{set, ?ALL bxor ?BREAKS}, R, N};
atom(<<"\\b", R/binary>>, _, N) -> {{edge, true}, R, N};
atom(<<"\\B", R/binary>>, _, N) -> {{edge, false}, R, N};
atom(<<"(?:", R/binary>>, Fl, N) -> group(R, Fl, N, fun(Alt) -> Alt end);
atom(<<"(?=", R/binary>>, Fl, N) -> look(R, Fl, N, true);
atom(<<"(?!", R/binary>>, Fl, N) -> look(R, Fl, N, false);
atom(<<"(?<", C, R/binary>>, Fl, N) when C =/= $=, C =/= $! ->
    [_, Body] = binary:split(R, <<">">>),
    group(Body, Fl, N + 1, fun(Alt) -> {capture, N, Alt} end);
atom(<<"(?", _/binary>>, _, _) -> throw(unsupported);
atom(<<$(, R/binary>>, Fl, N) -> group(R, Fl, N + 1, fun(Alt) -> {capture, N, Alt} end);
atom(<<"[^", R/binary>>, Fl, N) ->
    {Set, Rest} = class(R, Fl),
    {{set, (?ALL band bnot Set) bor ?BEYOND}, Rest, N};
atom(<<$[, R/binary>>, Fl, N) ->
    {Set, Rest} = class(R, Fl),
    {{set, Set}, Rest, N};
atom(<<C, _/binary>>, _, _)
  when C =:= ${; C =:= $}; C =:= $]; C =:= $*; C =:= $+; C =:= $? ->
    throw(unsupported);
atom(Bin, Fl, N) ->
    case item(Bin) of
        {{set, _} = Set, Rest} -> {Set, Rest, N};
        {C, Rest} -> {{set, span(C, C, Fl)}, Rest, N}
    end.

group(Bin, Fl, N, Wrap) ->
    {Alt, <<$), Rest/binary>>, N1} = alt(Bin, Fl, N),
    {Wrap(merged(Alt)), Rest, N1}.

%% a choice of single characters is a set
merged({alt, Seqs} = Alt) ->
    case [Set || [{set, Set}] <- Seqs] of
        Sets when length(Sets) =:= length(Seqs) ->
            {set, lists:foldl(fun(A, B) -> A bor B end, 0, Sets)};
        _ -> Alt
    end.

look(Bin, Fl, N, Wanted) ->
    case group(Bin, Fl, N, fun(Alt) -> {look, Wanted, Alt} end) of
        {_, _, N} = Look -> Look;
        _ -> throw(unsupported)
    end.

%% a character, or the set an escape stands for
item(<<"\\d", R/binary>>) -> {{set, ?DIGITS}, R};
item(<<"\\D", R/binary>>) -> {{set, ?ALL bxor ?DIGITS}, R};
item(<<"\\w", R/binary>>) -> {{set, ?WORD}, R};
item(<<"\\W", R/binary>>) -> {{set, ?ALL bxor ?WORD}, R};
item(<<"\\s", R/binary>>) -> {{set, ?SPACE}, R};
item(<<"\\S", R/binary>>) -> {{set, ?ALL bxor ?SPACE}, R};
item(<<"\\n", R/binary>>) -> {$\n, R};
item(<<"\\r", R/binary>>) -> {$\r, R};
item(<<"\\t", R/binary>>) -> {$\t, R};
item(<<"\\f", R/binary>>) -> {$\f, R};
item(<<"\\v", R/binary>>) -> {$\v, R};
item(<<"\\x", Hex:2/binary, R/binary>>) -> {hex(Hex), R};
item(<<"\\u", Hex:4/binary, R/binary>>) -> {hex(Hex), R};
item(<<$\\, C, R/binary>>) when C < 128, ?WORD band ?BIT(C) =:= 0 -> {C, R};
item(<<$\\, _/binary>>) -> throw(unsupported);
item(<<C/utf8, R/binary>>) when C < 16#10000 -> {C, R};
item(_) -> throw(unsupported).

hex(Digits) ->
    case [C || <<C>> <= Digits,
               C > 127 orelse ?BIT(C) band (?DIGITS bor ?SPAN($a, $f) bor ?SPAN($A, $F)) =:= 0] of
        [] ->
            case binary_to_integer(Digits, 16) of
                C when C >= 16#D800, C =< 16#DFFF -> throw(unsupported);
                C -> C
            end;
        _ -> throw(unsupported)
    end.

class(<<$], _/binary>>, _) -> throw(unsupported);
class(Bin, Fl) -> class(Bin, Fl, 0).

class(<<$], R/binary>>, _, Set) -> {Set, R};
%% pcre reads these as the start of a posix class
class(<<$[, C, _/binary>>, _, _) when C =:= $:; C =:= $.; C =:= $= -> throw(unsupported);
class(Bin, Fl, Set) ->
    case class_item(Bin) of
        {{set, _}, <<$-, C, _/binary>>} when C =/= $] -> throw(unsupported);
        {{set, More}, R} -> class(R, Fl, Set bor More);
        {Lo, <<$-, C, _/binary>> = R} when C =/= $] ->
            <<_, R1/binary>> = R,
            case class_item(R1) of
                {Hi, R2} when is_integer(Hi), Lo =< Hi -> class(R2, Fl, Set bor span(Lo, Hi, Fl));
                _ -> throw(unsupported)
            end;
        {C, R} -> class(R, Fl, Set bor span(C, C, Fl))
    end.

class_item(<<"\\b", R/binary>>) -> {$\b, R};
class_item(Bin) -> item(Bin).

%% a range as a set. a few letters past ascii have a case within it
span(_, Hi, #flags{caseless = true}) when Hi > 127 -> throw(unsupported);
span(Lo, Hi, _) when Hi > 127 -> ?SPAN(min(Lo, 128), 128);
span(Lo, Hi, #flags{caseless = Caseless}) ->
    Set = ?SPAN(Lo, Hi),
    case Caseless of
        true -> Set bor ((Set band ?UPPER) bsl 32) bor ((Set band ?LOWER) bsr 32);
        false -> Set
    end.

quantified(Atom, <<$*, R/binary>>) -> repeat(Atom, 0, many, R);
quantified(Atom, <<$+, R/binary>>) -> repeat(Atom, 1, many, R);
quantified(Atom, <<$?, R/binary>>) -> repeat(Atom, 0, 1, R);
quantified(Atom, <<${, R/binary>>) ->
    case count(R) of
        {Min, <<$}, R1/binary>>} -> repeat(Atom, Min, Min, R1);
        {Min, <<",}", R1/binary>>} -> repeat(Atom, Min, many, R1);
        {Min, <<$,, R1/binary>>} ->
            {Max, <<$}, R2/binary>>} = count(R1),
            repeat(Atom, Min, Max, R2);
        _ -> throw(unsupported)
    end;
quantified(Atom, R) -> {Atom, R}.

count(<<C, _/binary>> = Bin) when C >= $0, C =< $9 -> count(Bin, 0);
count(_) -> throw(unsupported).

count(<<C, R/binary>>, N) when C >= $0, C =< $9 -> count(R, N * 10 + C - $0);
count(R, N) -> {N, R}.

%% many, being an atom, is greater than any count
repeat(Atom, Min, Max, R) ->
    (Max =:= 0 orelse Min > Max orelse nullable(Atom) orelse (Max > 1 andalso captures(Atom)))
        andalso throw(unsupported),
    case R of
        <<$?, R1/binary>> -> {{repeat, Atom, Min, Max, lazy}, R1};
        _ -> {{repeat, Atom, Min, Max, greedy}, R}
    end.

%% what can be told ahead of a run

nullable({set, _}) -> false;
nullable({alt, Seqs}) -> lists:any(fun(Seq) -> lists:all(fun nullable/1, Seq) end, Seqs);
nullable({capture, _, Alt}) -> nullable(Alt);
nullable({repeat, _, Min, _, _}) -> Min =:= 0;
nullable(_) -> true.

captures({capture, _, _}) -> true;
captures({alt, Seqs}) -> lists:any(fun(Seq) -> lists:any(fun captures/1, Seq) end, Seqs);
captures({repeat, Atom, _, _, _}) -> captures(Atom);
captures(_) -> false.

anchored({alt, Seqs}) -> lists:all(fun([Term | _]) -> anchored(Term); ([]) -> false end, Seqs);
anchored({capture, _, Alt}) -> anchored(Alt);
anchored(Term) -> Term =:= text_start.

%% the characters a match of the term and then of what follows can start with,
%% given those of what follows. any when it could be none at all; the end of
%% the text is followed by nothing
first({set, Set}, _) -> Set;
first({alt, Seqs}, Next) ->
    lists:foldl(fun(Seq, Acc) -> either(lists:foldr(fun first/2, Next, Seq), Acc) end, 0, Seqs);
first({capture, _, Alt}, Next) -> first(Alt, Next);
first({repeat, Atom, 0, _, _}, Next) -> either(first(Atom, any), Next);
first({repeat, Atom, _, _, _}, _) -> first(Atom, any);
first({look, true, Alt}, Next) ->
    case nullable(Alt) of
        true -> Next;
        false -> first(Alt, any)
    end;
first(text_end, _) -> {ends, 0};
first(_, Next) -> Next.

either(any, _) -> any;
either(_, any) -> any;
either({ends, A}, B) -> ending(A, B);
either(A, {ends, B}) -> ending(B, A);
either(A, B) -> A bor B.

ending(A, {ends, B}) -> {ends, A bor B};
ending(A, B) -> {ends, A bor B}.

%% a set as three small integers
words(any) -> any;
words({ends, _}) -> any;
words(Set) -> {Set band ?SPAN(0, 47), (Set bsr 48) band ?SPAN(0, 47), Set bsr 96}.

%% building. A matcher is fun(Text, P, Bin, Caps, Fuel, Back): Text lists the
%% bytes from P on, and Back holds what follows the choices and repeats being
%% run, so that no matcher has two holders and copying one never multiplies it.
%% It gives [End | Caps], or the fuel left when it fails. K is the matcher for
%% what follows, and Next what that can start with

-define(MATCHER, fun(L, P, B, Caps, Fuel, Back)).
-define(IN(C),
        ((C < 48 andalso (W0 bsr C) band 1 =:= 1)
         orelse (C >= 96 andalso (W2 bsr (C - 96)) band 1 =:= 1)
         orelse (C >= 48 andalso C < 96 andalso (W1 bsr (C - 48)) band 1 =:= 1))).
-define(SPENT(Fuel), (if Fuel > 0 -> Fuel - 1; true -> throw(give_up) end)).

node({alt, [Seq]}, K, Next) -> sequence(Seq, K, Next);
node({alt, Seqs}, K, Next) ->
    Ways = [{words(lists:foldr(fun first/2, Next, Seq)), sequence(Seq, fun go_back/6, Next)}
            || Seq <- Seqs],
    ?MATCHER -> either_way(Ways, L, P, B, Caps, Fuel, [K | Back]) end;
node({set, Set}, K, _) when Set > 0, Set < ?BEYOND, Set band (Set - 1) =:= 0 ->
    Only = position(Set, 0),
    fun([C | L], P, B, Caps, Fuel, Back) when C =:= Only -> K(L, P + 1, B, Caps, Fuel, Back);
       (more, _, _, _, _, _) -> throw(more);
       (_, _, _, _, Fuel, _) -> Fuel
    end;
node({set, Set}, K, _) ->
    {W0, W1, W2} = words(Set),
    fun([C | L], P, B, Caps, Fuel, Back) when is_integer(C), C >= 0, C < 128 ->
            case ?IN(C) of
                true -> K(L, P + 1, B, Caps, Fuel, Back);
                false -> Fuel
            end;
       ([], _, _, _, Fuel, _) -> Fuel;
       (L, _, _, _, _, _) -> stop(L)
    end;
node({capture, N, Alt}, K, Next) ->
    Close = ?MATCHER ->
        {_, Start} = lists:keyfind(-N, 1, Caps),
        K(L, P, B, [{N, Start, P - Start} | Caps], Fuel, Back)
    end,
    Body = node(Alt, Close, Next),
    ?MATCHER -> Body(L, P, B, [{-N, P} | Caps], Fuel, Back) end;
node({look, Wanted, Alt}, K, _) ->
    Body = node(Alt, fun(_, _, _, _, Fuel, _) -> {Fuel} end, any),
    ?MATCHER ->
        case Body(L, P, B, Caps, Fuel, []) of
            {Left} when Wanted -> K(L, P, B, Caps, Left, Back);
            {Left} -> Left;
            Left when Wanted -> Left;
            Left -> K(L, P, B, Caps, Left, Back)
        end
    end;
node(text_start, K, _) ->
    fun(L, 0, B, Caps, Fuel, Back) -> K(L, 0, B, Caps, Fuel, Back);
       (_, _, _, _, Fuel, _) -> Fuel
    end;
node(text_end, K, _) ->
    fun([], P, B, Caps, Fuel, Back) -> K([], P, B, Caps, Fuel, Back);
       (more, _, _, _, _, _) -> throw(more);
       (_, _, _, _, Fuel, _) -> Fuel
    end;
node(line_start, K, _) ->
    ?MATCHER ->
        case before(B, P) of
            C when C =:= none; C =:= $\n; C =:= $\r -> K(L, P, B, Caps, Fuel, Back);
            _ -> Fuel
        end
    end;
node(line_end, K, _) ->
    ?MATCHER ->
        case upcoming(L) of
            C when C =:= none; C =:= $\n; C =:= $\r -> K(L, P, B, Caps, Fuel, Back);
            _ -> Fuel
        end
    end;
node({edge, Wanted}, K, _) ->
    ?MATCHER ->
        case is_word(before(B, P)) =/= is_word(upcoming(L)) of
            Wanted -> K(L, P, B, Caps, Fuel, Back);
            _ -> Fuel
        end
    end;
node({repeat, {set, Set}, Min, Max, greedy}, K, Next) ->
    {W0, W1, W2} = words(Set),
    case apart(Set, Next) of
        true ->
            ?MATCHER ->
                take_all(L, P, B, Caps, Fuel, Back, K, W0, W1, W2, P + Min, past(P, Max))
            end;
        false ->
            ?MATCHER -> take(L, P, B, Caps, Fuel, Back, K, W0, W1, W2, P + Min, past(P, Max)) end
    end;
node({repeat, {set, Set}, Min, Max, lazy}, K, Next) ->
    Taken = words(Set),
    %% no use stopping at these
    Passed = case Next of
        any -> {0, 0, 0};
        {ends, Starts} -> words(Set band bnot Starts);
        Starts -> words(Set band bnot Starts)
    end,
    ?MATCHER -> take_few(L, P, B, Caps, Fuel, Back, K, Taken, Passed, P + Min, past(P, Max)) end;
node({repeat, Atom, 0, 1, greedy}, K, Next) -> node({alt, [[Atom], []]}, K, Next);
node({repeat, Atom, 0, 1, lazy}, K, Next) -> node({alt, [[], [Atom]]}, K, Next);
node({repeat, Atom, Min, Max, Greed}, K, Next) ->
    Body = node(Atom, fun go_back/6, either(first(Atom, any), Next)),
    Loop = {Body, K, Min, Max, Greed},
    ?MATCHER -> loop(L, P, B, Caps, Fuel, Back, Loop, 0) end.

go_back(L, P, B, Caps, Fuel, [K | Back]) -> K(L, P, B, Caps, Fuel, Back).

either_way([{{W0, W1, W2}, _} | Ways], [C | _] = L, P, B, Caps, Fuel, Back)
  when is_integer(C), C >= 0, C < 128, not ?IN(C) ->
    either_way(Ways, L, P, B, Caps, Fuel, Back);
either_way([{_, Way}], L, P, B, Caps, Fuel, Back) -> Way(L, P, B, Caps, Fuel, Back);
either_way([{_, Way} | Ways], L, P, B, Caps, Fuel, Back) ->
    case Way(L, P, B, Caps, Fuel, Back) of
        Left when is_integer(Left) -> either_way(Ways, L, P, B, Caps, ?SPENT(Left), Back);
        Match -> Match
    end;
either_way([], _, _, _, _, Fuel, _) -> Fuel.

sequence(Terms, K, Next) ->
    Step = fun(Term, {K1, Next1}) -> {node(Term, K1, Next1), first(Term, Next1)} end,
    element(1, lists:foldr(Step, {K, Next}, Terms)).

position(1, N) -> N;
position(Set, N) when Set band 16#FFFF =:= 0 -> position(Set bsr 16, N + 16);
position(Set, N) -> position(Set bsr 1, N + 1).

%% giving a character back cannot help when what follows could not take it
apart(_, any) -> false;
apart(Set, {ends, Next}) -> Set band Next =:= 0;
apart(Set, Next) -> Set band Next =:= 0.

past(_, many) -> many;
past(P, Max) -> P + Max.

take_all([C | L1] = L, P, B, Caps, Fuel, Back, K, W0, W1, W2, Least, Most)
  when is_integer(C), C >= 0, C < 128 ->
    case P =/= Most andalso ?IN(C) of
        true -> take_all(L1, P + 1, B, Caps, Fuel, Back, K, W0, W1, W2, Least, Most);
        false when P >= Least -> K(L, P, B, Caps, Fuel, Back);
        false -> Fuel
    end;
take_all([], P, B, Caps, Fuel, Back, K, _, _, _, Least, _) when P >= Least ->
    K([], P, B, Caps, Fuel, Back);
take_all([], _, _, _, Fuel, _, _, _, _, _, _, _) -> Fuel;
take_all(L, _, _, _, _, _, _, _, _, _, _, _) -> stop(L).

take([C | L1] = L, P, B, Caps, Fuel, Back, K, W0, W1, W2, Least, Most)
  when is_integer(C), C >= 0, C < 128 ->
    case P =/= Most andalso ?IN(C) of
        true ->
            case take(L1, P + 1, B, Caps, Fuel, Back, K, W0, W1, W2, Least, Most) of
                Left when is_integer(Left), P >= Least -> K(L, P, B, Caps, ?SPENT(Left), Back);
                Other -> Other
            end;
        false when P >= Least -> K(L, P, B, Caps, Fuel, Back);
        false -> Fuel
    end;
take([], P, B, Caps, Fuel, Back, K, _, _, _, Least, _) when P >= Least ->
    K([], P, B, Caps, Fuel, Back);
take([], _, _, _, Fuel, _, _, _, _, _, _, _) -> Fuel;
take(L, _, _, _, _, _, _, _, _, _, _, _) -> stop(L).

take_few([C | L1] = L, P, B, Caps, Fuel, Back, K, Taken, {W0, W1, W2} = Passed, Least, Most)
  when is_integer(C), C >= 0, C < 128 ->
    if P =:= Most -> K(L, P, B, Caps, Fuel, Back);
       ?IN(C) -> take_few(L1, P + 1, B, Caps, Fuel, Back, K, Taken, Passed, Least, Most);
       P < Least -> take_one(L, P, B, Caps, Fuel, Back, K, Taken, Passed, Least, Most);
       true ->
           case K(L, P, B, Caps, Fuel, Back) of
               Left when is_integer(Left) ->
                   take_one(L, P, B, Caps, ?SPENT(Left), Back, K, Taken, Passed, Least, Most);
               Match -> Match
           end
    end;
take_few([], P, B, Caps, Fuel, Back, K, _, _, Least, _) when P >= Least ->
    K([], P, B, Caps, Fuel, Back);
take_few([], _, _, _, Fuel, _, _, _, _, _, _) -> Fuel;
take_few(L, _, _, _, _, _, _, _, _, _, _) -> stop(L).

take_one([C | L], P, B, Caps, Fuel, Back, K, {W0, W1, W2} = Taken, Passed, Least, Most)
  when ?IN(C) ->
    take_few(L, P + 1, B, Caps, Fuel, Back, K, Taken, Passed, Least, Most);
take_one(_, _, _, _, Fuel, _, _, _, _, _, _) -> Fuel.

loop(L, P, B, Caps, Fuel, Back, {_, K, _, N, _}, N) -> K(L, P, B, Caps, Fuel, Back);
loop(L, P, B, Caps, Fuel, Back, {Body, K, Min, _, Greed} = Loop, N) ->
    Go = fun(L1, P1, B1, Caps1, Fuel1, Back1) ->
        loop(L1, P1, B1, Caps1, Fuel1, Back1, Loop, N + 1)
    end,
    if N < Min -> Body(L, P, B, Caps, Fuel, [Go | Back]);
       Greed =:= greedy ->
           case Body(L, P, B, Caps, Fuel, [Go | Back]) of
               Left when is_integer(Left) -> K(L, P, B, Caps, ?SPENT(Left), Back);
               Match -> Match
           end;
       true ->
           case K(L, P, B, Caps, Fuel, Back) of
               Left when is_integer(Left) -> Body(L, P, B, Caps, ?SPENT(Left), [Go | Back]);
               Match -> Match
           end
    end.

before(_, 0) -> none;
before(B, P) ->
    case binary:at(B, P - 1) of
        C when C < 128 -> C;
        _ -> throw(give_up)
    end.

upcoming([C | _]) when C < 128 -> C;
upcoming([]) -> none;
upcoming(L) -> stop(L).

is_word(C) when C >= $a, C =< $z; C >= $A, C =< $Z; C >= $0, C =< $9; C =:= $_ -> true;
is_word(_) -> false.

%% running. {match, [whole | groups]} as re:run/3 has it, nomatch, or give_up

run({true, _, _}, _, Offset, _, _) when Offset > 0 -> nomatch;
run({Anchored, Match, First}, Bin, Offset, Sticky, Groups) when Anchored; Sticky ->
    case {First, Bin} of
        {{W0, W1, W2}, <<_:Offset/binary, C, _/binary>>} when C < 128, not ?IN(C) -> nomatch;
        _ -> within(?GLANCE, Match, Bin, Offset, Groups)
    end;
run({_, Match, First}, Bin, Offset, _, Groups) when byte_size(Bin) - Offset =< ?WINDOW ->
    try search(window(Bin, Offset, ?WINDOW), Offset, Bin, Match, First, Groups, ?FUEL)
    catch throw:give_up -> give_up
    end;
run(_, _, _, _, _) -> give_up.

%% most runs from a known start are over in a few bytes
within(Bytes, Match, Bin, Offset, Groups) ->
    try at(Match, window(Bin, Offset, Bytes), Offset, Bin, Groups)
    catch
        throw:more when Bytes < ?WINDOW -> within(?WINDOW, Match, Bin, Offset, Groups);
        throw:_ -> give_up
    end.

window(Bin, Offset, _) when Offset =:= byte_size(Bin) -> [];
window(Bin, Offset, Bytes) when byte_size(Bin) - Offset =< Bytes ->
    binary_to_list(Bin, Offset + 1, byte_size(Bin));
window(Bin, Offset, Bytes) -> binary_to_list(Bin, Offset + 1, Offset + Bytes) ++ more.

%% for what a matcher cannot go on with
stop(more) -> throw(more);
stop(_) -> throw(give_up).

at(Match, L, P, Bin, Groups) ->
    case Match(L, P, Bin, [], ?FUEL, []) of
        [End | Caps] -> {match, [{P, End - P} | groups(1, Groups, Caps)]};
        _ -> nomatch
    end.

search([C | L1] = L, P, Bin, Match, {W0, W1, W2} = First, Groups, Fuel)
  when is_integer(C), C >= 0, C < 128 ->
    case ?IN(C) of
        true -> attempt(L, P, Bin, Match, First, Groups, Fuel);
        false -> search(L1, P + 1, Bin, Match, First, Groups, Fuel)
    end;
search([], _, _, _, {_, _, _}, _, _) -> nomatch;
search([C | _], _, _, _, _, _, _) when C >= 128 -> throw(give_up);
search(L, P, Bin, Match, First, Groups, Fuel) -> attempt(L, P, Bin, Match, First, Groups, Fuel).

attempt(L, P, Bin, Match, First, Groups, Fuel) ->
    case Match(L, P, Bin, [], Fuel, []) of
        [End | Caps] -> {match, [{P, End - P} | groups(1, Groups, Caps)]};
        _ when L =:= [] -> nomatch;
        Left -> search(tl(L), P + 1, Bin, Match, First, Groups, Left)
    end.

groups(N, Groups, _) when N > Groups -> [];
groups(N, Groups, Caps) ->
    case lists:keyfind(N, 1, Caps) of
        {_, Start, Length} -> [{Start, Length} | groups(N + 1, Groups, Caps)];
        false -> [{-1, 0} | groups(N + 1, Groups, Caps)]
    end.
