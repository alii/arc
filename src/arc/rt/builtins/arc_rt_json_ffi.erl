%% §25.5.1 json text to json.gleam's JsonValue, by offset into one binary
-module(arc_rt_json_ffi).
-export([parse_value/2, plain_props/3, plain_keys/1, quote/1]).

-include("../arc_rt_layout.hrl").
-include("../arc_rt_names.hrl").

-define(DIGIT(C), (C >= $0 andalso C =< $9)).
-define(MAX_ARRAY_INDEX, 4294967294).

%% §10.1.11 order over a props map: indices ascending, names by seq
plain_keys(Props) ->
    {Idx, Named} = maps:fold(fun plain_key/3, {[], []}, Props),
    NamedKeys = [K || {_, K} <- lists:keysort(1, Named)],
    case Idx of
        [] -> NamedKeys;
        %% index keys are negative, ascending index is descending key
        _ -> lists:reverse(lists:sort(Idx), NamedKeys)
    end.

%% enumerable and seq sit at the same positions in both property shapes
plain_key(_, Prop, Acc) when element(?DATAPROP_ENUMERABLE, Prop) =:= false ->
    Acc;
plain_key(K, _, {Idx, Named}) when K < 0 ->
    {[K | Idx], Named};
plain_key(K, Prop, {Idx, Named}) when K band 3 =:= 0 ->
    {Idx, [{element(?DATAPROP_SEQ, Prop), K} | Named]};
plain_key(_, _, Acc) ->
    Acc.

%% §25.5.2.3 quotejsonstring as iodata
quote(Bin) ->
    case clean(Bin, 0) of
        true -> [$", Bin, $"];
        P -> [$", binary:part(Bin, 0, P) | quote_esc(Bin, P)]
    end.

-define(SAFE(C), (C =/= $" andalso C =/= $\\ andalso C >= 16#20)).

clean(Bin, P) ->
    case Bin of
        <<_:P/binary, A, B, C, D, E, F, G, H, _/binary>>
          when ?SAFE(A), ?SAFE(B), ?SAFE(C), ?SAFE(D), ?SAFE(E), ?SAFE(F),
               ?SAFE(G), ?SAFE(H) ->
            clean(Bin, P + 8);
        <<_:P/binary, A, B, C, D, _/binary>>
          when ?SAFE(A), ?SAFE(B), ?SAFE(C), ?SAFE(D) ->
            clean(Bin, P + 4);
        <<_:P/binary, A, _/binary>> when ?SAFE(A) -> clean(Bin, P + 1);
        <<_:P/binary>> -> true;
        _ -> P
    end.

%% p sits on a byte that needs escaping
quote_esc(Bin, P) ->
    <<_:P/binary, C, _/binary>> = Bin,
    Esc = case C of
        $" -> <<"\\\"">>;
        $\\ -> <<"\\\\">>;
        $\b -> <<"\\b">>;
        $\t -> <<"\\t">>;
        $\n -> <<"\\n">>;
        $\f -> <<"\\f">>;
        $\r -> <<"\\r">>;
        _ -> [<<"\\u00">>, hexc(C bsr 4), hexc(C band 15)]
    end,
    P1 = P + 1,
    case clean(Bin, P1) of
        true -> [Esc, binary:part(Bin, P1, byte_size(Bin) - P1), $"];
        P2 -> [Esc, binary:part(Bin, P1, P2 - P1) | quote_esc(Bin, P2)]
    end.

hexc(N) when N < 10 -> $0 + N;
hexc(N) -> $a + N - 10.

%% own data props, store written once per object; miss on duplicate keys
plain_props(Store, Entries, Seq) ->
    T = element(?STORE_NAMES, Store),
    Next = element(?NAMES_NEXT, T),
    plain_props(Entries, Seq, Seq, [], element(?NAMES_NUMBERS, T),
                element(?NAMES_TEXTS, T), Next, Next, Store).

plain_props([{Name, V} | Rest], Seq, Seq0, Acc, Nums, Texts, Next, Next0, Store) ->
    Prop = {?DATAPROP_TAG, V, true, true, true, Seq},
    case arc_rt_val_ffi:index_of_text(Name) of
        none ->
            case Nums of
                #{Name := N} ->
                    plain_props(Rest, Seq + 1, Seq0, [{?NAME_KEY(N), Prop} | Acc],
                                Nums, Texts, Next, Next0, Store);
                #{} ->
                    B = binary:copy(Name),
                    plain_props(Rest, Seq + 1, Seq0, [{?NAME_KEY(Next), Prop} | Acc],
                                Nums#{B => Next}, Texts#{?NAME_KEY(Next) => B},
                                Next + 1, Next0, Store)
            end;
        I ->
            plain_props(Rest, Seq + 1, Seq0, [{?INDEX_KEY(I), Prop} | Acc],
                        Nums, Texts, Next, Next0, Store)
    end;
plain_props([], Seq, Seq0, Acc, Nums, Texts, Next, Next0, Store) ->
    Map = maps:from_list(Acc),
    case map_size(Map) =:= Seq - Seq0 of
        true when Next =:= Next0 -> {some, {Map, Seq, Store}};
        true ->
            T = element(?STORE_NAMES, Store),
            T1 = setelement(?NAMES_NEXT,
                     setelement(?NAMES_TEXTS, setelement(?NAMES_NUMBERS, T, Nums), Texts),
                     Next),
            Alloc = element(?STORE_ALLOC, Store) + Next - Next0,
            {some, {Map, Seq, setelement(?STORE_ALLOC, setelement(?STORE_NAMES, Store, T1), Alloc)}};
        false -> none
    end.
-define(PLAIN(C), (C =/= $" andalso C =/= $\\ andalso C >= 16#20)).

%% src: keep source slices for the reviver context
parse_value(Bin, Src) ->
    try value(Bin, ws(Bin, 0), Src) of
        {V, P} -> {ok, {V, binary:part(Bin, P, byte_size(Bin) - P)}}
    catch
        throw:{json, Reason} -> {error, Reason}
    end.

ws(Bin, P) ->
    case Bin of
        <<_:P/binary, C, _/binary>>
          when C =:= $\s; C =:= $\t; C =:= $\n; C =:= $\r ->
            ws(Bin, P + 1);
        _ -> P
    end.

value(Bin, P, Src) ->
    case Bin of
        <<_:P/binary, $", _/binary>> ->
            {Str, End} = str(Bin, P + 1, P + 1),
            Source = case Src of
                true -> binary:part(Bin, P, End - P);
                false -> <<>>
            end,
            {{json_string, Str, Source}, End};
        <<_:P/binary, ${, _/binary>> -> object(Bin, ws(Bin, P + 1), Src, []);
        <<_:P/binary, $[, _/binary>> -> array(Bin, ws(Bin, P + 1), Src, []);
        <<_:P/binary, "null", _/binary>> -> {{json_null, <<"null">>}, P + 4};
        <<_:P/binary, "true", _/binary>> -> {{json_bool, true, <<"true">>}, P + 4};
        <<_:P/binary, "false", _/binary>> ->
            {{json_bool, false, <<"false">>}, P + 5};
        <<_:P/binary, C, _/binary>> when C =:= $-; ?DIGIT(C) -> number(Bin, P, Src);
        <<_:P/binary>> -> fail(unexpected_end);
        <<_:P/binary, C/utf8, _/binary>> -> fail({unexpected_token, <<C/utf8>>});
        _ -> fail(invalid_utf8)
    end.

fail(Reason) -> throw({json, Reason}).

%% p is past the opening quote, start marks the pending literal run
str(Bin, Start, P) ->
    case Bin of
        <<_:P/binary, A, B, C, D, E, F, G, H, _/binary>>
          when ?PLAIN(A), ?PLAIN(B), ?PLAIN(C), ?PLAIN(D), ?PLAIN(E), ?PLAIN(F),
               ?PLAIN(G), ?PLAIN(H) ->
            str(Bin, Start, P + 8);
        <<_:P/binary, $", _/binary>> ->
            {binary:part(Bin, Start, P - Start), P + 1};
        <<_:P/binary, $\\, _/binary>> ->
            escape(Bin, P + 1, binary:part(Bin, Start, P - Start));
        <<_:P/binary, C, _/binary>> when C < 16#20 ->
            fail(control_char_in_string);
        <<_:P/binary, _, _/binary>> -> str(Bin, Start, P + 1);
        _ -> fail(unterminated_string)
    end.

str_acc(Bin, Start, P, Acc) ->
    case Bin of
        <<_:P/binary, $", _/binary>> ->
            {iolist_to_binary([Acc | binary:part(Bin, Start, P - Start)]),
             P + 1};
        <<_:P/binary, $\\, _/binary>> ->
            escape(Bin, P + 1, [Acc | binary:part(Bin, Start, P - Start)]);
        <<_:P/binary, C, _/binary>> when C < 16#20 ->
            fail(control_char_in_string);
        <<_:P/binary, _, _/binary>> -> str_acc(Bin, Start, P + 1, Acc);
        _ -> fail(unterminated_string)
    end.

escape(Bin, P, Acc) ->
    case Bin of
        <<_:P/binary, C, _/binary>> when C =:= $"; C =:= $\\; C =:= $/ ->
            str_acc(Bin, P + 1, P + 1, [Acc, C]);
        <<_:P/binary, $b, _/binary>> -> str_acc(Bin, P + 1, P + 1, [Acc, $\b]);
        <<_:P/binary, $f, _/binary>> -> str_acc(Bin, P + 1, P + 1, [Acc, $\f]);
        <<_:P/binary, $n, _/binary>> -> str_acc(Bin, P + 1, P + 1, [Acc, $\n]);
        <<_:P/binary, $r, _/binary>> -> str_acc(Bin, P + 1, P + 1, [Acc, $\r]);
        <<_:P/binary, $t, _/binary>> -> str_acc(Bin, P + 1, P + 1, [Acc, $\t]);
        <<_:P/binary, $u, _/binary>> ->
            {Utf8, P1} = unicode_escape(Bin, P + 1),
            str_acc(Bin, P1, P1, [Acc | Utf8]);
        <<_:P/binary>> -> fail(unterminated_escape);
        <<_:P/binary, C/utf8, _/binary>> -> fail({invalid_escape, <<C/utf8>>});
        _ -> fail(unterminated_escape)
    end.

unicode_escape(Bin, P) ->
    Cp = hex4(Bin, P),
    P1 = P + 4,
    if
        Cp >= 16#D800, Cp =< 16#DBFF ->
            case Bin of
                <<_:P1/binary, $\\, $u, _/binary>> ->
                    case low_surrogate(Bin, P1 + 2) of
                        none -> {<<16#FFFD/utf8>>, P1};
                        Low ->
                            U = 16#10000 + (Cp - 16#D800) * 1024 + (Low - 16#DC00),
                            {<<U/utf8>>, P1 + 6}
                    end;
                _ -> {<<16#FFFD/utf8>>, P1}
            end;
        Cp >= 16#DC00, Cp =< 16#DFFF -> {<<16#FFFD/utf8>>, P1};
        true -> {<<Cp/utf8>>, P1}
    end.

low_surrogate(Bin, P) ->
    case Bin of
        <<_:P/binary, A, B, C, D, _/binary>> ->
            case hex(A) bor hex(B) bor hex(C) bor hex(D) of
                Bad when Bad > 15 -> none;
                _ ->
                    Low = (hex(A) bsl 12) bor (hex(B) bsl 8) bor (hex(C) bsl 4)
                        bor hex(D),
                    case Low >= 16#DC00 andalso Low =< 16#DFFF of
                        true -> Low;
                        false -> none
                    end
            end;
        _ -> none
    end.

hex4(Bin, P) ->
    case Bin of
        <<_:P/binary, A, B, C, D, _/binary>> ->
            case hex(A) bor hex(B) bor hex(C) bor hex(D) of
                Bad when Bad > 15 -> fail(invalid_unicode_escape);
                _ ->
                    (hex(A) bsl 12) bor (hex(B) bsl 8) bor (hex(C) bsl 4)
                        bor hex(D)
            end;
        _ -> fail(invalid_unicode_escape)
    end.

hex(C) when C >= $0, C =< $9 -> C - $0;
hex(C) when C >= $a, C =< $f -> C - $a + 10;
hex(C) when C >= $A, C =< $F -> C - $A + 10;
hex(_) -> 16.

number(Bin, P, Src) ->
    P1 = case Bin of
        <<_:P/binary, $-, _/binary>> -> P + 1;
        _ -> P
    end,
    {P2, Int} = case Bin of
        <<_:P1/binary, $0, D, _/binary>> when ?DIGIT(D) -> bad_number(Bin, P);
        <<_:P1/binary, $0, _/binary>> -> {P1 + 1, 0};
        <<_:P1/binary, D1, _/binary>> when ?DIGIT(D1) ->
            int_digits(Bin, P1 + 1, D1 - $0);
        _ -> bad_number(Bin, P)
    end,
    P3 = case Bin of
        <<_:P2/binary, $., D2, _/binary>> when ?DIGIT(D2) -> digits(Bin, P2 + 2);
        <<_:P2/binary, $., _/binary>> -> bad_number(Bin, P);
        _ -> P2
    end,
    P4 = case Bin of
        <<_:P3/binary, E, S, D3, _/binary>>
          when (E =:= $e orelse E =:= $E), (S =:= $+ orelse S =:= $-),
               ?DIGIT(D3) ->
            digits(Bin, P3 + 3);
        <<_:P3/binary, E, D3, _/binary>>
          when (E =:= $e orelse E =:= $E), ?DIGIT(D3) ->
            digits(Bin, P3 + 2);
        <<_:P3/binary, E, _/binary>> when E =:= $e; E =:= $E ->
            bad_number(Bin, P);
        _ -> P3
    end,
    Num = case P4 =:= P2 andalso P2 - P1 =< 15 of
        true when Int =:= 0, P1 > P -> {j_float, -0.0};
        true when P1 > P -> {j_int, -Int};
        true -> {j_int, Int};
        false -> 'arc@rt@val':string_to_number(binary:part(Bin, P, P4 - P))
    end,
    Source = case Src of
        true -> binary:part(Bin, P, P4 - P);
        false -> <<>>
    end,
    {{json_number, Num, Source}, P4}.

int_digits(Bin, P, Acc) ->
    case Bin of
        <<_:P/binary, D, _/binary>> when ?DIGIT(D) ->
            int_digits(Bin, P + 1, Acc * 10 + (D - $0));
        _ -> {P, Acc}
    end.

digits(Bin, P) ->
    case Bin of
        <<_:P/binary, D, _/binary>> when ?DIGIT(D) -> digits(Bin, P + 1);
        _ -> P
    end.

bad_number(Bin, P) ->
    fail({invalid_number, binary:part(Bin, P, number_ish(Bin, P) - P)}).

number_ish(Bin, P) ->
    case Bin of
        <<_:P/binary, C, _/binary>>
          when C =:= $-; C =:= $+; C =:= $.; C =:= $e; C =:= $E; ?DIGIT(C) ->
            number_ish(Bin, P + 1);
        _ -> P
    end.

array(Bin, P, Src, Acc) ->
    case Bin of
        <<_:P/binary, $], _/binary>> -> {{json_array, lists:reverse(Acc)}, P + 1};
        <<_:P/binary>> -> fail(unterminated_array);
        _ ->
            P1 = case Acc of
                [] -> P;
                _ ->
                    case Bin of
                        <<_:P/binary, $,, _/binary>> -> ws(Bin, P + 1);
                        _ -> fail({expected, <<"',' or ']'">>, <<"array">>})
                    end
            end,
            {V, P2} = value(Bin, P1, Src),
            array(Bin, ws(Bin, P2), Src, [V | Acc])
    end.

object(Bin, P, Src, Acc) ->
    case Bin of
        <<_:P/binary, $}, _/binary>> ->
            {{json_object, lists:reverse(Acc)}, P + 1};
        <<_:P/binary>> -> fail(unterminated_object);
        _ ->
            P1 = case Acc of
                [] -> P;
                _ ->
                    case Bin of
                        <<_:P/binary, $,, _/binary>> -> ws(Bin, P + 1);
                        _ -> fail({expected, <<"',' or '}'">>, <<"object">>})
                    end
            end,
            {Key, P2} = case Bin of
                <<_:P1/binary, $", _/binary>> -> str(Bin, P1 + 1, P1 + 1);
                _ -> fail({expected, <<"string key">>, <<"object">>})
            end,
            Colon = ws(Bin, P2),
            P3 = case Bin of
                <<_:Colon/binary, $:, _/binary>> -> ws(Bin, Colon + 1);
                _ -> fail({expected, <<"':' after key">>, <<"object">>})
            end,
            {V, P4} = value(Bin, P3, Src),
            object(Bin, ws(Bin, P4), Src, [{Key, V} | Acc])
    end.
