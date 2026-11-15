%% §25.5.1 json text to json.gleam's JsonValue, by offset into one binary.
%% plain_stringify may return json_miss
-module(arc_rt_json_ffi).
-export([parse_value/2, plain_prop_dict/2, plain_enumerable_keys/1, quote_tree/1,
         plain_stringify/3, plain_parse/2]).

-include("../arc_rt_layout.hrl").

-define(DIGIT(C), (C >= $0 andalso C =< $9)).
-define(IS_FUNCTION_KIND(Kind),
        element(1, Kind) =:= ?BYTECODEFN_TAG; element(1, Kind) =:= ?NATIVEFN_TAG;
        element(1, Kind) =:= ?COMPILEDFN_TAG; element(1, Kind) =:= ?BOUNDFN_TAG).

%% §10.1.11 order over a props map: indices ascending, names by seq
plain_enumerable_keys(Props) ->
    {Idx, Named} = maps:fold(fun plain_key/3, {[], []}, Props),
    NamedKeys = [K || {_, K} <- lists:keysort(1, Named)],
    case Idx of
        [] -> NamedKeys;
        _ -> [{?KEY_INDEX, I} || I <- lists:sort(Idx)] ++ NamedKeys
    end.

%% enumerable and seq sit at the same positions in both property records
plain_key(Slot, Entry, {Idx, Named} = Acc) ->
    Prop = element(2, Entry),
    case element(?DATAPROPERTY_ENUMERABLE, Prop) of
        false -> Acc;
        true ->
            case arc_rt_prop_map_ffi:key_at(Slot, Entry) of
                {?KEY_INDEX, I} -> {[I | Idx], Named};
                {?KEY_NAMED, _, _} = K ->
                    {Idx, [{element(?DATAPROPERTY_SEQ, Prop), K} | Named]};
                _ -> Acc
            end
    end.

%% §25.5.2.3 quotejsonstring as iodata
quote_tree(Bin) ->
    case clean(Bin, 0) of
        true -> [$", Bin, $"];
        P -> [$", binary:part(Bin, 0, P) | quote_esc(Bin, P)]
    end.

-define(SAFE(C), (C =/= $" andalso C =/= $\\ andalso C >= 16#20)).

clean(Bin, P) ->
    <<_:P/binary, Rest/binary>> = Bin,
    clean_from(Rest, P).

clean_from(<<A, B, C, D, Rest/binary>>, P) when ?SAFE(A), ?SAFE(B), ?SAFE(C), ?SAFE(D) ->
    clean_from(Rest, P + 4);
clean_from(<<A, Rest/binary>>, P) when ?SAFE(A) -> clean_from(Rest, P + 1);
clean_from(<<>>, _) -> true;
clean_from(_, P) -> P.

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

%% own data props from parsed entries, or props_miss when two keys want one slot
plain_prop_dict(Entries, Seq) -> plain_props(Entries, Seq, []).

plain_props([{Name, V} | Rest], Seq, Acc) ->
    Prop = ?PLAIN_PROPERTY(V, Seq),
    plain_props(Rest, Seq + 1, [entry(Name, Prop) | Acc]);
plain_props([], Seq, Acc) ->
    Map = maps:from_list(Acc),
    case map_size(Map) =:= length(Acc) of
        true -> {plain_props, Map, Seq};
        false -> props_miss
    end.

entry(<<C, _/binary>> = B, Prop) when ?DIGIT(C) ->
    try binary_to_integer(B) of
        N when N >= 0, N =< ?MAX_ARRAY_INDEX ->
            case integer_to_binary(N) =:= B of
                true -> {?INDEX_SLOT(N), ?INDEX_ENTRY(Prop)};
                false -> named_entry(B, Prop)
            end;
        _ -> named_entry(B, Prop)
    catch
        error:badarg -> named_entry(B, Prop)
    end;
entry(B, Prop) -> named_entry(B, Prop).

named_entry(B, Prop) -> {arc_bytecode_key_ffi:hash(B) * 2, {B, Prop}}.
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
        <<_:P/binary, $n, _/binary>> -> literal(Bin, P + 1, <<"ull">>);
        <<_:P/binary, $t, _/binary>> -> literal(Bin, P + 1, <<"rue">>);
        <<_:P/binary, $f, _/binary>> -> literal(Bin, P + 1, <<"alse">>);
        <<_:P/binary>> -> fail(unexpected_end, P);
        <<_:P/binary, _/utf8, _/binary>> -> fail(unexpected_character, P);
        _ -> fail(invalid_utf8, P)
    end.

%% a literal that went wrong. fail at the first byte that differs
literal(Bin, P, <<C, Rest/binary>>) ->
    case Bin of
        <<_:P/binary, C, _/binary>> -> literal(Bin, P + 1, Rest);
        <<_:P/binary>> -> fail(unexpected_end, P);
        _ -> fail(unexpected_character, P)
    end.

%% p is a byte offset into the text
fail(Kind, P) -> throw({json, {parse_failure, Kind, P}}).

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
            fail(bad_control_character, P);
        <<_:P/binary, _, _/binary>> -> str(Bin, Start, P + 1);
        _ -> fail(unterminated_string, P)
    end.

str_acc(Bin, Start, P, Acc) ->
    case Bin of
        <<_:P/binary, $", _/binary>> ->
            {iolist_to_binary([Acc | binary:part(Bin, Start, P - Start)]),
             P + 1};
        <<_:P/binary, $\\, _/binary>> ->
            escape(Bin, P + 1, [Acc | binary:part(Bin, Start, P - Start)]);
        <<_:P/binary, C, _/binary>> when C < 16#20 ->
            fail(bad_control_character, P);
        <<_:P/binary, _, _/binary>> -> str_acc(Bin, Start, P + 1, Acc);
        _ -> fail(unterminated_string, P)
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
        <<_:P/binary>> -> fail(unexpected_end, P);
        <<_:P/binary, C/utf8, _/binary>> when C > 16#FF ->
            fail(unexpected_character, P);
        _ -> fail(bad_escaped_character, P)
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
                Bad when Bad > 15 -> fail(bad_unicode_escape, not_hex(Bin, P));
                _ ->
                    (hex(A) bsl 12) bor (hex(B) bsl 8) bor (hex(C) bsl 4)
                        bor hex(D)
            end;
        _ -> fail(bad_unicode_escape, not_hex(Bin, P))
    end.

not_hex(Bin, P) ->
    case Bin of
        <<_:P/binary, C, _/binary>> ->
            case hex(C) of
                16 -> P;
                _ -> not_hex(Bin, P + 1)
            end;
        _ -> P
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
        <<_:P1/binary, $0, D, _/binary>> when ?DIGIT(D) ->
            fail(unexpected_character, P1 + 1);
        <<_:P1/binary, $0, _/binary>> -> {P1 + 1, 0};
        <<_:P1/binary, D1, _/binary>> when ?DIGIT(D1) ->
            int_digits(Bin, P1 + 1, D1 - $0);
        _ -> fail(no_number_after_minus, P1)
    end,
    P3 = case Bin of
        <<_:P2/binary, $., D2, _/binary>> when ?DIGIT(D2) -> digits(Bin, P2 + 2);
        <<_:P2/binary, $., _/binary>> -> fail(unterminated_fraction, P2 + 1);
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
        <<_:P3/binary, E, S, _/binary>>
          when (E =:= $e orelse E =:= $E), (S =:= $+ orelse S =:= $-) ->
            fail(exponent_missing_number, P3 + 2);
        <<_:P3/binary, E, _/binary>> when E =:= $e; E =:= $E ->
            fail(exponent_missing_number, P3 + 1);
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

array(Bin, P, Src, Acc) ->
    case Bin of
        <<_:P/binary, $], _/binary>> -> {{json_array, lists:reverse(Acc)}, P + 1};
        _ ->
            P1 = case Acc of
                [] -> P;
                _ ->
                    case Bin of
                        <<_:P/binary, $,, _/binary>> -> ws(Bin, P + 1);
                        _ -> fail(expected_comma_or_bracket, P)
                    end
            end,
            {V, P2} = value(Bin, P1, Src),
            array(Bin, ws(Bin, P2), Src, [V | Acc])
    end.

object(Bin, P, Src, Acc) ->
    case Bin of
        <<_:P/binary, $}, _/binary>> ->
            {{json_object, lists:reverse(Acc)}, P + 1};
        _ ->
            P1 = case Acc of
                [] -> P;
                _ ->
                    case Bin of
                        <<_:P/binary, $,, _/binary>> -> ws(Bin, P + 1);
                        _ -> fail(expected_comma_or_brace, P)
                    end
            end,
            {Key, P2} = case {Bin, Acc} of
                {<<_:P1/binary, $", _/binary>>, _} -> str(Bin, P1 + 1, P1 + 1);
                {_, []} -> fail(expected_property_name_or_brace, P1);
                _ -> fail(expected_property_name, P1)
            end,
            Colon = ws(Bin, P2),
            P3 = case Bin of
                <<_:Colon/binary, $:, _/binary>> -> ws(Bin, Colon + 1);
                _ -> fail(expected_colon, Colon)
            end,
            {V, P4} = value(Bin, P3, Src),
            object(Bin, ws(Bin, P4), Src, [{Key, V} | Acc])
    end.

%% §25.5.1 with no reviver, straight to cells in one pass over the text. text
%% with anything wrong in it returns parse_miss, for parse_value/2 to say what.
%% Stack holds the arrays and objects still open, and Seen the lists of keys
%% met once so far
plain_parse(St, Bin) ->
    try p_value(Bin, Bin, 0, [], St, #{})
    catch throw:{json, _} -> parse_miss
    end.

-define(WS(C), (C =:= $\s orelse C =:= $\n orelse C =:= $\t orelse C =:= $\r)).
-define(ASCII(C), (C >= 16#20 andalso C < 16#80 andalso C =/= $" andalso C =/= $\\)).

p_value(<<$", R/binary>>, Bin, P, Stack, St, Seen) ->
    p_string(R, Bin, P + 1, 0, Stack, St, Seen);
p_value(<<C, R/binary>>, Bin, P, Stack, St, Seen) when C >= $1, C =< $9 ->
    p_int(R, Bin, P, 1, C - $0, Stack, St, Seen);
p_value(<<${, R/binary>>, Bin, P, Stack, St, Seen) -> p_first_key(R, Bin, P + 1, Stack, St, Seen);
p_value(<<$[, R/binary>>, Bin, P, Stack, St, Seen) -> p_first_item(R, Bin, P + 1, Stack, St, Seen);
p_value(<<"true", R/binary>>, Bin, P, Stack, St, Seen) ->
    p_next(R, Bin, P + 4, Stack, St, Seen, true);
p_value(<<"false", R/binary>>, Bin, P, Stack, St, Seen) ->
    p_next(R, Bin, P + 5, Stack, St, Seen, false);
p_value(<<"null", R/binary>>, Bin, P, Stack, St, Seen) ->
    p_next(R, Bin, P + 4, Stack, St, Seen, null);
p_value(<<C, R/binary>>, Bin, P, Stack, St, Seen) when ?WS(C) ->
    p_value(R, Bin, P + 1, Stack, St, Seen);
p_value(<<C, _/binary>>, Bin, P, Stack, St, Seen) when C =:= $-; C =:= $0 ->
    p_number(Bin, P, Stack, St, Seen);
p_value(_, _, _, _, _, _) -> parse_miss.

p_int(<<C, R/binary>>, Bin, P, Len, N, Stack, St, Seen) when ?DIGIT(C), Len < 15 ->
    p_int(R, Bin, P, Len + 1, N * 10 + (C - $0), Stack, St, Seen);
p_int(<<C, _/binary>>, Bin, P, _, _, Stack, St, Seen)
  when ?DIGIT(C); C =:= $.; C =:= $e; C =:= $E ->
    p_number(Bin, P, Stack, St, Seen);
p_int(R, Bin, P, Len, N, Stack, St, Seen) -> p_next(R, Bin, P + Len, Stack, St, Seen, N).

p_number(Bin, P, Stack, St, Seen) ->
    {{json_number, Num, _}, P1} = number(Bin, P, false),
    <<_:P1/binary, R/binary>> = Bin,
    p_next(R, Bin, P1, Stack, St, Seen, arc_rt_val_ffi:mk_number(Num)).

p_string(<<A, B, C, D, R/binary>>, Bin, P, Len, Stack, St, Seen)
  when ?ASCII(A), ?ASCII(B), ?ASCII(C), ?ASCII(D) ->
    p_string(R, Bin, P, Len + 4, Stack, St, Seen);
p_string(<<$", R/binary>>, Bin, P, Len, Stack, St, Seen) ->
    p_next(R, Bin, P + Len + 1, Stack, St, Seen, binary_part(Bin, P, Len));
p_string(<<C, R/binary>>, Bin, P, Len, Stack, St, Seen) when ?ASCII(C) ->
    p_string(R, Bin, P, Len + 1, Stack, St, Seen);
p_string(_, Bin, P, Len, Stack, St, Seen) ->
    {Text, P1} = str(Bin, P, P + Len),
    <<_:P1/binary, R/binary>> = Bin,
    Str = case Stack of
        [key | _] -> Text;
        _ -> arc_rt_js_string_ffi:from_text(Text)
    end,
    p_next(R, Bin, P1, Stack, St, Seen, Str).

p_first_item(<<$], R/binary>>, Bin, P, Stack, St, Seen) ->
    p_made(R, Bin, P + 1, Stack, St, Seen, array_cell(St, []));
p_first_item(<<C, R/binary>>, Bin, P, Stack, St, Seen) when ?WS(C) ->
    p_first_item(R, Bin, P + 1, Stack, St, Seen);
p_first_item(R, Bin, P, Stack, St, Seen) -> p_value(R, Bin, P, [item, [] | Stack], St, Seen).

p_first_key(<<$}, R/binary>>, Bin, P, Stack, St, Seen) ->
    Cell = {?SSHAPEDOBJECT_TAG, ?PLAIN_EMPTY_SHAPE, object_proto(St), {}, #{}},
    p_made(R, Bin, P + 1, Stack, St, Seen, Cell);
p_first_key(<<C, R/binary>>, Bin, P, Stack, St, Seen) when ?WS(C) ->
    p_first_key(R, Bin, P + 1, Stack, St, Seen);
p_first_key(R, Bin, P, Stack, St, Seen) -> p_key(R, Bin, P, [[], [] | Stack], St, Seen).

p_key(<<$", R/binary>>, Bin, P, Stack, St, Seen) ->
    p_string(R, Bin, P + 1, 0, [key | Stack], St, Seen);
p_key(<<C, R/binary>>, Bin, P, Stack, St, Seen) when ?WS(C) ->
    p_key(R, Bin, P + 1, Stack, St, Seen);
p_key(_, _, _, _, _, _) -> parse_miss.

%% V has just been read; what may follow depends on what it is in
p_next(<<C, R/binary>>, Bin, P, Stack, St, Seen, V) ->
    case Stack of
        _ when ?WS(C) -> p_next(R, Bin, P + 1, Stack, St, Seen, V);
        [item, Items | Below] when C =:= $, ->
            p_value(R, Bin, P + 1, [item, [V | Items] | Below], St, Seen);
        [item, Items | Below] when C =:= $] ->
            p_made(R, Bin, P + 1, Below, St, Seen, array_cell(St, lists:reverse(Items, [V])));
        [key, Keys | Below] when C =:= $: ->
            p_value(R, Bin, P + 1, [member, [V | Keys] | Below], St, Seen);
        [member, Keys, Values | Below] when C =:= $, ->
            p_key(R, Bin, P + 1, [Keys, [V | Values] | Below], St, Seen);
        [member, Keys, Values | Below] when C =:= $} ->
            case object_cell(St, Seen, Keys, [V | Values]) of
                {Cell, St1, Seen1} -> p_made(R, Bin, P + 1, Below, St1, Seen1, Cell);
                miss -> parse_miss
            end;
        _ -> parse_miss
    end;
p_next(<<>>, _, _, [], St, _, V) -> {parsed, V, St};
p_next(_, _, _, _, _, _, _) -> parse_miss.

p_made(<<R/binary>>, Bin, P, Stack, St, Seen, Cell) when tuple_size(St) =:= ?AGENT_SIZE ->
    Id = element(?AGENT_NEXT_ID, St),
    St1 = ?ALLOC_CELL(St, element(?AGENT_CELLS, St), Id, Cell),
    St2 = case Cell of
        {?SOBJECT_TAG, {?ARRAYOBJ_TAG, Len}, _, _, _, _, _} when Len > 64 ->
            arc_rt_store_ffi:weighed(St1, Len);
        _ -> St1
    end,
    p_next(R, Bin, P, Stack, St2, Seen, {?HANDLE_TAG, Id}).

object_proto(St) -> ?REALM_SHARES(?REALMSHARED_OBJECT_PROTO, element(?AGENT_REALM, St)).

array_cell(St, Items) ->
    Els = case Items of
        [] -> ?ELEMS_NONE;
        _ -> {?ELEMS_DENSE, arc_tree_array_ffi:from_list(Items)}
    end,
    {?SOBJECT_TAG, {?ARRAYOBJ_TAG, length(Items)},
     ?REALM_SHARES(?REALMSHARED_ARRAY_PROTO, element(?AGENT_REALM, St)), #{}, [], Els, true}.

-define(SHAPES_TO_SPARE, 4096).

%% Keys and Values come last first. a shape is there for good, so keys get one
%% only when they turn up a second time, as a record's do, and only while there
%% are few enough shapes that text nobody vouches for cannot fill memory with them
object_cell(St, Seen, Keys, Values) ->
    Step = iolist_to_binary(joined(Keys, [])),
    case ?SHAPED_NEXT(?STORED(?STORE_SHAPES, St), 0, Step) of
        {Shape, Offs} -> {shaped_cell(St, Shape, Offs, Values), St, Seen};
        miss when is_map_key(Step, Seen), ?STORED(?STORE_NEXT_SHAPE, St) < ?SHAPES_TO_SPARE ->
            case shape_keys(Keys, [], #{?KEY_ID(?PROTO_KEY) => nil}) of
                miss -> dict_cell(St, Seen, Keys, Values);
                Named ->
                    {Shape, Offs, St1} = 'arc@rt@obj':literal_shape(St, Named),
                    {shaped_cell(St1, Shape, Offs, Values), St1, Seen}
            end;
        miss -> dict_cell(St, Seen#{Step => nil}, Keys, Values)
    end.

%% as rt/obj.literal_shape files a list of keys
joined([Key | Keys], Acc) -> joined(Keys, [255, Key | Acc]);
joined([], Acc) -> Acc.

%% names with ids of their own. the shape outlives the text, so it gets copies
shape_keys([<<C, _/binary>> | _], _, _) when ?DIGIT(C) -> miss;
shape_keys([Text | Texts], Acc, Ids) when map_size(Ids) =< 32 ->
    case arc_bytecode_key_ffi:named(binary:copy(Text)) of
        K when is_map_key(?KEY_ID(K), Ids) -> miss;
        K -> shape_keys(Texts, [K | Acc], Ids#{?KEY_ID(K) => nil})
    end;
shape_keys([], Acc, _) -> Acc;
shape_keys(_, _, _) -> miss.

shaped_cell(St, Shape, Offs, Values) ->
    {?SSHAPEDOBJECT_TAG, Shape, object_proto(St), list_to_tuple(lists:reverse(Values)), Offs}.

dict_cell(St, Seen, Keys, Values) when tuple_size(St) =:= ?AGENT_SIZE ->
    Entries = lists:reverse(lists:zip(Keys, Values)),
    case plain_prop_dict(Entries, element(?AGENT_PROP_SEQ, St)) of
        {plain_props, Props, Seq} ->
            {{?SOBJECT_TAG, ?ORDINARY, object_proto(St), Props, [], ?ELEMS_NONE, true},
             setelement(?AGENT_PROP_SEQ, St, Seq), Seen};
        props_miss -> miss
    end.

%% §25.5.2 for plain data with no replacer. anything observable returns json_miss
plain_stringify(St, V, Gap) ->
    Realm = element(?AGENT_REALM, St),
    Cells = element(?AGENT_CELLS, St),
    {?HANDLE_TAG, OP} = element(?BUILTINPAIR_PROTOTYPE, element(?REALM_OBJECT, Realm)),
    {?HANDLE_TAG, AP} = element(?BUILTINPAIR_PROTOTYPE, element(?REALM_ARRAY, Realm)),
    TJ = ?KEY_ID(?TO_JSON_KEY),
    Clean = fun(Id) ->
        case arc_rt_arena_ffi:get(Id, Cells) of
            {?SOBJECT_TAG, _, _, Props, _, _, _} -> not is_map_key(TJ, Props);
            _ -> false
        end
    end,
    case Clean(OP) andalso Clean(AP) of
        false -> json_miss;
        true when Gap =:= <<>> ->
            try tight(V, {Cells, OP, AP, Gap}, [], <<>>, #{}) of
                skip -> json_miss;
                {Text, _} -> {json_done, Text}
            catch throw:json_miss -> json_miss
            end;
        true ->
            try enc(V, {Cells, OP, AP, Gap}, <<>>, []) of
                skip -> json_miss;
                Io -> {json_done, iolist_to_binary(Io)}
            catch throw:json_miss -> json_miss
            end
    end.

%% enc/4 with no gap, appending to Acc. Heads holds what each shape met so far
%% writes before its values
tight(S, _, _, Acc, Heads) when is_binary(S) -> {quoted(S, Acc), Heads};
tight(N, _, _, Acc, Heads) when is_integer(N) ->
    {<<Acc/binary, (integer_to_binary(N))/binary>>, Heads};
tight({?HANDLE_TAG, Id} = V, {Cells, OP, AP, _} = Cx, Seen, Acc, Heads) ->
    case arc_rt_arena_ffi:get(Id, Cells) of
        {?SSHAPEDOBJECT_TAG, Shape, {?SOME, {?HANDLE_TAG, OP}}, Slots, Offs}
          when not is_map_key(?KEY_ID(?TO_JSON_KEY), Offs) ->
            {Names, Heads1} = case Heads of
                #{Shape := Known} -> {Known, Heads};
                _ ->
                    Made = [<<(quoted(?KEY_TEXT(K), <<>>))/binary, $:>>
                            || {K, _} <- lists:keysort(2, maps:values(Offs))],
                    {Made, Heads#{Shape => Made}}
            end,
            members(Names, tuple_to_list(Slots), Cx, entered(Id, Seen), Acc, Heads1, ${);
        {?SOBJECT_TAG, ?ORDINARY, {?SOME, {?HANDLE_TAG, OP}}, Props, _, ?ELEMS_NONE, _}
          when not is_map_key(?KEY_ID(?TO_JSON_KEY), Props) ->
            {Keys, Values} = lists:unzip(plain_pairs(Props)),
            Names = [<<(quoted(K, <<>>))/binary, $:>> || K <- Keys],
            members(Names, Values, Cx, entered(Id, Seen), Acc, Heads, ${);
        {?SOBJECT_TAG, {?ARRAYOBJ_TAG, Len}, {?SOME, {?HANDLE_TAG, AP}}, Props, _, Els, _}
          when map_size(Props) =:= 0 ->
            items(elems(Els, Len), Cx, entered(Id, Seen), Acc, Heads, $[);
        _ -> appended(enc(V, Cx, <<>>, Seen), Acc, Heads)
    end;
tight(V, Cx, Seen, Acc, Heads) -> appended(enc(V, Cx, <<>>, Seen), Acc, Heads).

appended(skip, _, _) -> skip;
appended(Text, Acc, Heads) when is_binary(Text) -> {<<Acc/binary, Text/binary>>, Heads};
appended(Io, Acc, Heads) -> {<<Acc/binary, (iolist_to_binary(Io))/binary>>, Heads}.

entered(Id, Seen) ->
    case lists:member(Id, Seen) of
        true -> throw(json_miss);
        false -> [Id | Seen]
    end.

quoted(S, Acc) ->
    case clean(S, 0) of
        true -> <<Acc/binary, $", S/binary, $">>;
        P -> <<Acc/binary, $", S:P/binary, (iolist_to_binary(quote_esc(S, P)))/binary>>
    end.

%% Before is the bracket until something has been written, then the comma
members([Name | Names], [V | Values], Cx, Seen, Acc, Heads, Before) ->
    case is_skipped(V, Cx) of
        true -> members(Names, Values, Cx, Seen, Acc, Heads, Before);
        false ->
            {Acc1, Heads1} = tight(V, Cx, Seen, <<Acc/binary, Before, Name/binary>>, Heads),
            members(Names, Values, Cx, Seen, Acc1, Heads1, $,)
    end;
members([], [], _, _, Acc, Heads, $,) -> {<<Acc/binary, $}>>, Heads};
members([], [], _, _, Acc, Heads, _) -> {<<Acc/binary, "{}">>, Heads}.

items([V | Values], Cx, Seen, Acc, Heads, Before) ->
    case is_skipped(V, Cx) of
        true -> items(Values, Cx, Seen, <<Acc/binary, Before, "null">>, Heads, $,);
        false ->
            {Acc1, Heads1} = tight(V, Cx, Seen, <<Acc/binary, Before>>, Heads),
            items(Values, Cx, Seen, Acc1, Heads1, $,)
    end;
items([], _, _, Acc, Heads, $,) -> {<<Acc/binary, $]>>, Heads};
items([], _, _, Acc, Heads, _) -> {<<Acc/binary, "[]">>, Heads}.

is_skipped(undefined, _) -> true;
is_skipped({js_sym, _}, _) -> true;
is_skipped({?HANDLE_TAG, Id}, {Cells, _, _, _}) ->
    case arc_rt_arena_ffi:get(Id, Cells) of
        {?SOBJECT_TAG, Kind, _, _, _, _, _} = Cell when ?IS_FUNCTION_KIND(Kind) ->
            without_to_json(Cells, Cell, ?MAX_PROTO_HOPS);
        _ -> false
    end;
is_skipped(_, _) -> false.

%% a function is left out only after its toJSON has been looked for
without_to_json(Cells, {?SOBJECT_TAG, Kind, Proto, Props, _, _, _}, Fuel)
  when Fuel > 0, not is_map_key(?KEY_ID(?TO_JSON_KEY), Props),
       (is_atom(Kind) orelse element(1, Kind) =/= ?PROXYOBJ_TAG) ->
    case Proto of
        {?SOME, {?HANDLE_TAG, Id}} ->
            without_to_json(Cells, arc_rt_arena_ffi:get(Id, Cells), Fuel - 1);
        ?NONE -> true
    end;
without_to_json(_, _, _) -> throw(json_miss).

enc(N, _, _, _) when is_integer(N) -> integer_to_binary(N);
enc(F, _, _, _) when is_float(F) -> arc_rt_val_ffi:js_format_float(F);
enc(true, _, _, _) -> <<"true">>;
enc(false, _, _, _) -> <<"false">>;
enc(null, _, _, _) -> <<"null">>;
enc(A, _, _, _) when A =:= js_nan; ?IS_INF(A) -> <<"null">>;
enc(undefined, _, _, _) -> skip;
enc({js_sym, _}, _, _, _) -> skip;
enc(S, _, _, _) when ?IS_STR(S) -> quote_tree(arc_rt_js_string_ffi:text(S));
enc({?HANDLE_TAG, Id}, {Cells, OP, AP, _} = Cx, Ind, Seen) ->
    case lists:member(Id, Seen) of
        true -> throw(json_miss);
        false -> ok
    end,
    case arc_rt_arena_ffi:get(Id, Cells) of
        {?SSHAPEDOBJECT_TAG, _, Proto, Slots, Offs}
          when (Proto =:= {?SOME, {?HANDLE_TAG, OP}} orelse Proto =:= ?NONE),
               not is_map_key(?KEY_ID(?TO_JSON_KEY), Offs) ->
            Pairs = [{?KEY_TEXT(K), ?SLOT_AT(Slots, Off)}
                     || {K, Off} <- lists:keysort(2, maps:values(Offs))],
            enc_object(Pairs, Cx, Ind, [Id | Seen]);
        {?SOBJECT_TAG, ?ORDINARY, Proto, Props, _, ?ELEMS_NONE, _}
          when (Proto =:= {?SOME, {?HANDLE_TAG, OP}} orelse Proto =:= ?NONE),
               not is_map_key(?KEY_ID(?TO_JSON_KEY), Props) ->
            enc_object(plain_pairs(Props), Cx, Ind, [Id | Seen]);
        {?SOBJECT_TAG, {?ARRAYOBJ_TAG, Len}, {?SOME, {?HANDLE_TAG, AP}}, Props, _, Els, _}
          when map_size(Props) =:= 0 ->
            enc_array(elems(Els, Len), Cx, Ind, [Id | Seen]);
        {?SOBJECT_TAG, Kind, _, _, _, _, _} = Cell when ?IS_FUNCTION_KIND(Kind) ->
            true = without_to_json(Cells, Cell, ?MAX_PROTO_HOPS),
            skip;
        _ -> throw(json_miss)
    end;
enc(_, _, _, _) -> throw(json_miss).

plain_pairs(Props) ->
    L = maps:to_list(Props),
    Idx = lists:sort([{-1 - Slot, element(2, E)} || {Slot, E} <- L, Slot < 0]),
    Named = lists:sort(fun({_, A}, {_, B}) ->
                element(?DATAPROPERTY_SEQ, A) =< element(?DATAPROPERTY_SEQ, B) end,
                       [{element(1, E), element(2, E)}
                        || {Slot, E} <- L, Slot >= 0, Slot band 1 =:= 0]),
    [{key_text(K), value_of(P)} || {K, P} <- Idx ++ Named, enumerable(P)].

key_text(N) when is_integer(N) -> integer_to_binary(N);
key_text(B) -> B.

value_of(P) when element(1, P) =:= ?DATAPROPERTY_TAG -> element(?DATAPROPERTY_VALUE, P);
value_of(_) -> throw(json_miss).

enumerable(P) when element(1, P) =:= ?DATAPROPERTY_TAG ->
    element(?DATAPROPERTY_ENUMERABLE, P) =:= true;
enumerable(_) -> throw(json_miss).

elems(_, 0) -> [];
elems({?ELEMS_DENSE, A}, Len) ->
    case arc_tree_array_ffi:dense_list(A, Len) of
        {?SOME, L} -> L;
        ?NONE -> throw(json_miss)
    end;
elems(_, _) -> throw(json_miss).

enc_object(Pairs, {_, _, _, Gap} = Cx, Ind, Seen) ->
    Ind1 = <<Ind/binary, Gap/binary>>,
    Members = [member(quote_tree(K), enc(V, Cx, Ind1, Seen), Gap) || {K, V} <- Pairs],
    case [M || M <- Members, M =/= skip] of
        [] -> <<"{}">>;
        Ms when Gap =:= <<>> -> [${, lists:join($,, Ms), $}];
        Ms -> [${, $\n, Ind1, lists:join([$,, $\n, Ind1], Ms), $\n, Ind, $}]
    end.

member(_, skip, _) -> skip;
member(K, V, <<>>) -> [K, $:, V];
member(K, V, _) -> [K, <<": ">>, V].

enc_array(Items, {_, _, _, Gap} = Cx, Ind, Seen) ->
    Ind1 = <<Ind/binary, Gap/binary>>,
    Vs = [case enc(V, Cx, Ind1, Seen) of skip -> <<"null">>; E -> E end || V <- Items],
    case Vs of
        [] -> <<"[]">>;
        _ when Gap =:= <<>> -> [$[, lists:join($,, Vs), $]];
        _ -> [$[, $\n, Ind1, lists:join([$,, $\n, Ind1], Vs), $\n, Ind, $]]
    end.
