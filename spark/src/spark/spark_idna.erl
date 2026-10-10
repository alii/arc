%% uts 46: a domain name to its ascii form, the way the url standard asks
%% (non-transitional, bidi and joiner checks on, hyphen and length checks off)
%% https://www.unicode.org/reports/tr46/
-module(spark_idna).
-export([to_ascii/1, to_unicode/1]).

%% code points in, ascii code points out; throws `failure`
to_ascii(Domain) ->
    Labels = [label(Label) || Label <- split(unicode:characters_to_nfc_list(map(Domain)))],
    check_bidi([Unicode || {Unicode, _} <- Labels]),
    lists:append(lists:join(".", [Ascii || {_, Ascii} <- Labels])).

%% an ascii name with each encoded label decoded, where it decodes to a
%% valid one
to_unicode(Ascii) ->
    lists:append(lists:join(".", [decoded_or_same(Label) || Label <- split(Ascii)])).

decoded_or_same("xn--" ++ Encoded = Label) ->
    try
        Decoded = punycode_decode(Encoded),
        valid_decoded(Decoded) orelse throw(failure),
        check_bidi([Decoded]),
        Decoded
    catch
        throw:failure -> Label
    end;
decoded_or_same(Label) -> Label.

%% ---- mapping

map([C | Rest]) ->
    case lookup(C, spark_idna_tables:mapping()) of
        v -> [C | map(Rest)];
        x -> [C | map(Rest)];
        i -> map(Rest);
        d -> throw(failure);
        To -> To ++ map(Rest)
    end;
map([]) -> [].

%% ---- labels: {as unicode, as ascii}

label("xn--" ++ Encoded = Label) ->
    is_ascii(Label) orelse throw(failure),
    Decoded = punycode_decode(Encoded),
    valid_decoded(Decoded) orelse throw(failure),
    {Decoded, Label};
label(Label) ->
    valid(Label) orelse throw(failure),
    case is_ascii(Label) of
        true -> {Label, Label};
        false -> {Label, "xn--" ++ punycode_encode(Label)}
    end.

%% what an encoded label has to decode to: something beyond ascii that
%% mapping would have left alone
valid_decoded(Decoded) ->
    Decoded =/= []
        andalso not is_ascii(Decoded)
        andalso not lists:prefix("xn--", Decoded)
        andalso unicode:characters_to_nfc_list(Decoded) =:= Decoded
        andalso lists:all(fun allowed/1, Decoded)
        andalso valid(Decoded).

allowed(C) ->
    case lookup(C, spark_idna_tables:mapping()) of
        v -> true;
        x -> true;
        _ -> false
    end.

valid([]) -> true;
valid([First | _] = Label) ->
    lookup(First, spark_idna_tables:marks()) =:= none andalso joiners_fit([], Label).

%% rfc 5892 appendix a: a zero width joiner needs a virama before it; a non-
%% joiner needs that, or to sit between letters that join across it
joiners_fit(_Before, []) -> true;
joiners_fit(Before, [16#200D | After]) ->
    after_virama(Before) andalso joiners_fit([16#200D | Before], After);
joiners_fit(Before, [16#200C | After]) ->
    (after_virama(Before) orelse (joins_left(Before) andalso joins_right(After)))
        andalso joiners_fit([16#200C | Before], After);
joiners_fit(Before, [C | After]) ->
    joiners_fit([C | Before], After).

after_virama([C | _]) -> lookup(C, spark_idna_tables:viramas()) =/= none;
after_virama([]) -> false.

%% Before is nearest first
joins_left([C | Rest]) ->
    case lookup(C, spark_idna_tables:joining()) of
        t -> joins_left(Rest);
        l -> true;
        d -> true;
        _ -> false
    end;
joins_left([]) -> false.

joins_right([C | Rest]) ->
    case lookup(C, spark_idna_tables:joining()) of
        t -> joins_right(Rest);
        r -> true;
        d -> true;
        _ -> false
    end;
joins_right([]) -> false.

%% ---- rfc 5893's bidi rule, held to each label that has right-to-left
%% characters in it. The rfc holds every label of such a name to it; node
%% checks label by label, and this follows node

check_bidi(Labels) ->
    lists:all(fun(Label) ->
        Classes = [bidi(C) || C <- Label],
        not lists:any(fun right_to_left/1, Classes) orelse bidi_rule(Classes)
    end, Labels) orelse throw(failure),
    ok.

bidi(C) ->
    case lookup(C, spark_idna_tables:bidi()) of
        none -> l;
        Class -> Class
    end.

right_to_left(Class) -> Class =:= r orelse Class =:= al orelse Class =:= an.

bidi_rule([]) -> true;
bidi_rule([First | _] = Classes) when First =:= r; First =:= al ->
    lists:all(fun(C) -> lists:member(C, [r, al, an, en, es, cs, et, on, bn, nsm]) end, Classes)
        andalso lists:member(last_strong(Classes), [r, al, en, an])
        andalso not (lists:member(en, Classes) andalso lists:member(an, Classes));
bidi_rule([l | _] = Classes) ->
    lists:all(fun(C) -> lists:member(C, [l, en, es, cs, et, on, bn, nsm]) end, Classes)
        andalso lists:member(last_strong(Classes), [l, en]);
bidi_rule(_) -> false.

%% the last class, looking past trailing nonspacing marks
last_strong(Classes) ->
    case lists:dropwhile(fun(C) -> C =:= nsm end, lists:reverse(Classes)) of
        [Class | _] -> Class;
        [] -> nsm
    end.

%% ---- the tables are tuples of {First, Last, What} in order

lookup(C, Table) -> lookup(C, Table, 1, tuple_size(Table)).

lookup(_, _, Low, High) when Low > High -> none;
lookup(C, Table, Low, High) ->
    Mid = (Low + High) div 2,
    case element(Mid, Table) of
        {First, _, _} when C < First -> lookup(C, Table, Low, Mid - 1);
        {_, Last, _} when C > Last -> lookup(C, Table, Mid + 1, High);
        {_, _, What} -> What
    end.

is_ascii(Chars) -> lists:all(fun(C) -> C < 128 end, Chars).

split(Chars) -> split(Chars, [], []).

split([$. | Rest], Label, Labels) -> split(Rest, [], [lists:reverse(Label) | Labels]);
split([C | Rest], Label, Labels) -> split(Rest, [C | Label], Labels);
split([], Label, Labels) -> lists:reverse([lists:reverse(Label) | Labels]).

%% ---- punycode, rfc 3492

-define(BASE, 36).
-define(TMIN, 1).
-define(TMAX, 26).
-define(SKEW, 38).
-define(DAMP, 700).

punycode_encode(Input) ->
    Basic = [C || C <- Input, C < 128],
    Handled = length(Basic),
    Out = case Basic of
        [] -> [];
        _ -> [$- | lists:reverse(Basic)]
    end,
    puny_enc(Input, 128, 0, 72, Handled, Handled, Out).

%% Out is the result so far, newest first
puny_enc(Input, _N, _Delta, _Bias, _Basic, Handled, Out) when Handled >= length(Input) ->
    lists:reverse(Out);
puny_enc(Input, N, Delta0, Bias0, Basic, Handled0, Out0) ->
    M = lists:min([C || C <- Input, C >= N]),
    Delta1 = Delta0 + (M - N) * (Handled0 + 1),
    {Delta, Bias, Handled, Out} =
        lists:foldl(
            fun(C, {D, B, H, O}) when C < M -> {D + 1, B, H, O};
               (C, {D, B, H, O}) when C =:= M ->
                    O1 = puny_digits(D, ?BASE, B, O),
                    {0, adapt(D, H + 1, H =:= Basic), H + 1, O1};
               (_, Acc) -> Acc
            end,
            {Delta1, Bias0, Handled0, Out0}, Input),
    puny_enc(Input, M + 1, Delta + 1, Bias, Basic, Handled, Out).

puny_digits(Q, K, Bias, Out) ->
    T = threshold(K, Bias),
    case Q < T of
        true -> [puny_digit(Q) | Out];
        false ->
            puny_digits((Q - T) div (?BASE - T), K + ?BASE, Bias,
                        [puny_digit(T + (Q - T) rem (?BASE - T)) | Out])
    end.

threshold(K, Bias) when K =< Bias + ?TMIN -> ?TMIN;
threshold(K, Bias) when K >= Bias + ?TMAX -> ?TMAX;
threshold(K, Bias) -> K - Bias.

puny_digit(D) when D < 26 -> $a + D;
puny_digit(D) -> $0 + D - 26.

adapt(Delta, Points, First) ->
    D0 = case First of
        true -> Delta div ?DAMP;
        false -> Delta div 2
    end,
    adapt_loop(D0 + D0 div Points, 0).

adapt_loop(Delta, K) when Delta > ((?BASE - ?TMIN) * ?TMAX) div 2 ->
    adapt_loop(Delta div (?BASE - ?TMIN), K + ?BASE);
adapt_loop(Delta, K) ->
    K + ((?BASE - ?TMIN + 1) * Delta) div (Delta + ?SKEW).

punycode_decode(Input) ->
    {Basic, Rest} = case string:rchr(Input, $-) of
        0 -> {[], Input};
        At -> {lists:sublist(Input, At - 1), lists:nthtail(At, Input)}
    end,
    is_ascii(Basic) orelse throw(failure),
    puny_dec(Rest, Basic, 128, 0, 72).

puny_dec([], Out, _N, _I, _Bias) -> Out;
puny_dec(Input, Out, N0, I0, Bias0) ->
    {I1, Rest} = puny_number(Input, I0, 1, ?BASE, Bias0),
    Length = length(Out) + 1,
    Bias = adapt(I1 - I0, Length, I0 =:= 0),
    N = N0 + I1 div Length,
    (N > 16#10FFFF orelse (N >= 16#D800 andalso N =< 16#DFFF)) andalso throw(failure),
    I = I1 rem Length,
    {Before, After} = lists:split(I, Out),
    puny_dec(Rest, Before ++ [N | After], N, I + 1, Bias).

puny_number([], _I, _W, _K, _Bias) -> throw(failure);
puny_number([C | Rest], I, W, K, Bias) ->
    Digit = if
        C >= $a, C =< $z -> C - $a;
        C >= $A, C =< $Z -> C - $A;
        C >= $0, C =< $9 -> C - $0 + 26;
        true -> throw(failure)
    end,
    I1 = I + Digit * W,
    T = threshold(K, Bias),
    case Digit < T of
        true -> {I1, Rest};
        false -> puny_number(Rest, I1, W * (?BASE - T), K + ?BASE, Bias)
    end.
