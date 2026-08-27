-module(arc_regexp_ffi).
-export([regexp_exec_info/5]).
-export([regexp_compile/2, is_compiled/1, regexp_exec_compiled/4]).
-export([pair_trail/1]).

-define(CS, arc_regex_charset).

%% inclass: false | true | atom (prev item can start a range)
-define(IN_CLASS(X), (X =:= true orelse X =:= atom)).

%% m and s are desugared in translate_pat, never pcre options
flags_to_opts(Flags) ->
    flags_to_opts(Flags, [unicode]).

flags_to_opts(<<>>, Acc) -> Acc;
flags_to_opts(<<"i", Rest/binary>>, Acc) -> flags_to_opts(Rest, [caseless | Acc]);
flags_to_opts(<<_, Rest/binary>>, Acc) -> flags_to_opts(Rest, Acc).

newline_mode(Flags) -> newline_mode(Flags, false, false).

newline_mode(<<>>, M, S) -> {M, S};
newline_mode(<<"m", Rest/binary>>, _M, S) -> newline_mode(Rest, true, S);
newline_mode(<<"s", Rest/binary>>, M, _S) -> newline_mode(Rest, M, true);
newline_mode(<<_, Rest/binary>>, M, S) -> newline_mode(Rest, M, S).

get_compiled(Pattern, Flags) ->
    Opts = flags_to_opts(Flags),
    Mode = unicode_mode(Flags),
    NL = newline_mode(Flags),
    Caseless = lists:member(caseless, Opts),
    {Stripped, GroupCount, Names} = scan_pattern(Pattern),
    Translated = unicode:characters_to_binary(
                   leading_star_prefix(Stripped, NL)
                   ++ translate_pat(Stripped, false, Mode, Caseless, [NL])),
    case re:compile(Translated, Opts) of
        {ok, MP} ->
            {ok, {MP, GroupCount, Names}};
        {error, Reason} ->
            {error, {pattern_compile_failed, compile_reason(Reason)}}
    end.

compile_reason({Msg, Pos}) when is_list(Msg), is_integer(Pos) ->
    unicode:characters_to_binary(io_lib:format("~ts at ~b", [Msg, Pos]));
compile_reason(Other) ->
    unicode:characters_to_binary(io_lib:format("~tp", [Other])).

scan_pattern(Pattern) ->
    {ChunksRev, GroupCount, NamesRev} =
        scan(unicode:characters_to_list(Pattern), false, 0, [], []),
    Names = lists:reverse(NamesRev),
    Stripped = resolve_backrefs(lists:reverse(ChunksRev), index_by_name(Names)),
    {Stripped, GroupCount, Names}.

scan([], _InClass, N, Chunks, Names) ->
    {Chunks, N, Names};
scan([$\\, $k, $< | Rest], false, N, Chunks, Names) ->
    {Name, Rest2, Terminated} = take_group_name(Rest),
    Raw = "\\k<" ++ Name ++ case Terminated of true -> ">"; false -> "" end,
    Chunk = {backref, unicode:characters_to_binary(Name), Raw},
    scan(Rest2, false, N, [Chunk | Chunks], Names);
scan([$\\, C | Rest], InClass, N, Chunks, Names) ->
    scan(Rest, InClass, N, [C, $\\ | Chunks], Names);
scan([$[ | Rest], false, N, Chunks, Names) ->
    scan(Rest, true, N, [$[ | Chunks], Names);
scan([$] | Rest], true, N, Chunks, Names) ->
    scan(Rest, false, N, [$] | Chunks], Names);
scan([$(, $?, $<, C | Rest], false, N, Chunks, Names) when C =/= $=, C =/= $! ->
    {Name, Rest2, _Terminated} = take_group_name([C | Rest]),
    scan(Rest2, false, N + 1, [$( | Chunks],
         [{unicode:characters_to_binary(Name), N + 1} | Names]);
scan([$(, $? | Rest], false, N, Chunks, Names) ->
    scan(Rest, false, N, [$?, $( | Chunks], Names);
scan([$( | Rest], false, N, Chunks, Names) ->
    scan(Rest, false, N + 1, [$( | Chunks], Names);
scan([C | Rest], InClass, N, Chunks, Names) ->
    scan(Rest, InClass, N, [C | Chunks], Names).

index_by_name(Names) ->
    lists:foldl(
      fun({Name, Idx}, Acc) ->
              case lists:keyfind(Name, 1, Acc) of
                  {Name, Idxs} ->
                      lists:keyreplace(Name, 1, Acc, {Name, Idxs ++ [Idx]});
                  false ->
                      Acc ++ [{Name, [Idx]}]
              end
      end, [], Names).

resolve_backrefs(Chunks, ByName) ->
    lists:flatmap(fun(Chunk) -> resolve_chunk(Chunk, ByName) end, Chunks).

%% {pcre, _} chunks pass through translate_pat untouched
resolve_chunk({backref, Name, Raw}, ByName) ->
    case lists:keyfind(Name, 1, ByName) of
        {_, [Idx]} ->
            [{pcre, "\\g{" ++ integer_to_list(Idx) ++ "}"}];
        {_, Idxs} ->
            Refs = ["\\g{" ++ integer_to_list(I) ++ "}" || I <- Idxs],
            [{pcre, "(?:" ++ lists:append(lists:join("|", Refs)) ++ ")"}];
        false ->
            Raw
    end;
resolve_chunk(C, _ByName) when is_integer(C) ->
    [C].

unicode_mode(<<>>) -> none;
unicode_mode(<<"v", _/binary>>) -> v;
unicode_mode(<<"u", Rest/binary>>) ->
    case unicode_mode(Rest) of
        v -> v;
        _ -> u
    end;
unicode_mode(<<_, Rest/binary>>) -> unicode_mode(Rest).

-define(WORD_BODY, "0-9A-Za-z_").
-define(WORD, "[" ?WORD_BODY "]").
-define(NWORD, "[^" ?WORD_BODY "]").

-define(JSS_CHARS,
        "\\t\\n\\x0B\\f\\r \\x{A0}\\x{1680}\\x{2000}-\\x{200A}"
        "\\x{2028}\\x{2029}\\x{202F}\\x{205F}\\x{3000}\\x{FEFF}").

-define(JS_LT, "\\n\\r\\x{2028}\\x{2029}").

%% restores pcre startline optimisation lost by desugaring dot
leading_star_prefix([$., Star | _], {_Multiline, DotAll})
  when Star =:= $*; Star =:= $+ ->
    case DotAll of
        true -> "\\G";
        false -> "(?:\\G|(?<=[" ?JS_LT "]))"
    end;
leading_star_prefix(_Stripped, _NewlineMode) ->
    "".

translate_pat([], _InClass, _Mode, _CI, _MS) -> [];
translate_pat([$\\, $u, ${ | Rest], InClass, Mode, CI, MS) ->
    case take_hex(Rest, []) of
        {Hex, [$} | Rest2]} when Hex =/= [] ->
            case is_surrogate_hex(Hex) of
                true ->
                    emit_surrogate(?IN_CLASS(InClass), Rest2, Mode, CI, MS);
                false ->
                    [$\\, $x, ${] ++ Hex ++ [$}]
                        ++ translate_pat(Rest2, after_atom(InClass), Mode, CI, MS)
            end;
        _ ->
            [$\\, $u, ${ | translate_pat(Rest, InClass, Mode, CI, MS)]
    end;
translate_pat([$\\, $u, A, B, C, D | Rest], InClass, Mode, CI, MS) ->
    case is_hex(A) andalso is_hex(B) andalso is_hex(C) andalso is_hex(D) of
        true ->
            V = list_to_integer([A, B, C, D], 16),
            if
                V >= 16#D800, V =< 16#DBFF,
                (InClass =:= false orelse Mode =/= none) ->
                    case pair_trail(Rest) of
                        {ok, W, Rest2} ->
                            "\\x{" ++ integer_to_list(combine_surrogates(V, W), 16) ++ "}"
                                ++ translate_pat(Rest2, after_atom(InClass),
                                                 Mode, CI, MS);
                        none ->
                            emit_surrogate(?IN_CLASS(InClass), Rest, Mode, CI, MS)
                    end;
                V >= 16#D800, V =< 16#DFFF ->
                    emit_surrogate(?IN_CLASS(InClass), Rest, Mode, CI, MS);
                true ->
                    [$\\, $x, ${, A, B, C, D, $}
                     | translate_pat(Rest, after_atom(InClass), Mode, CI, MS)]
            end;
        false -> [$\\, $u | translate_pat([A, B, C, D | Rest], InClass, Mode, CI, MS)]
    end;
translate_pat([$\\, P, ${ | Rest], InClass, Mode, CI, MS)
  when (P =:= $p orelse P =:= $P), Mode =/= none ->
    case take_prop(Rest, []) of
        {Payload, Rest2} ->
            case prop_translation(Payload, P =:= $P, ?IN_CLASS(InClass), Mode) of
                {ok, Io} ->
                    unicode:characters_to_list(iolist_to_binary(Io))
                        ++ translate_pat(Rest2, after_class_item(InClass),
                                         Mode, CI, MS);
                error ->
                    [$\\, P, ${ | translate_pat(Rest, InClass, Mode, CI, MS)]
            end;
        none ->
            [$\\, P, ${ | translate_pat(Rest, InClass, Mode, CI, MS)]
    end;
translate_pat([$\\, $s | Rest], false, Mode, CI, MS) ->
    "[" ?JSS_CHARS "]" ++ translate_pat(Rest, false, Mode, CI, MS);
translate_pat([$\\, $S | Rest], false, Mode, CI, MS) ->
    "[^" ?JSS_CHARS "]" ++ translate_pat(Rest, false, Mode, CI, MS);
translate_pat([$\\, $w | Rest], false, Mode, CI, MS) ->
    word_atom(Mode) ++ translate_pat(Rest, false, Mode, CI, MS);
translate_pat([$\\, $W | Rest], false, Mode, CI, MS) ->
    nword_atom(Mode) ++ translate_pat(Rest, false, Mode, CI, MS);
translate_pat([$\\, $s | Rest], IC, Mode, CI, MS) when ?IN_CLASS(IC) ->
    splice_in_class(?JSS_CHARS, Rest, Mode, CI, MS);
translate_pat([$\\, $S | Rest], IC, Mode, CI, MS) when ?IN_CLASS(IC) ->
    splice_in_class(?CS:emit_complement(?CS:vspace(), CI), Rest, Mode, CI, MS);
translate_pat([$\\, $w | Rest], IC, Mode, CI, MS) when ?IN_CLASS(IC) ->
    splice_in_class(word_items(Mode), Rest, Mode, CI, MS);
translate_pat([$\\, $W | Rest], IC, Mode, CI, MS) when ?IN_CLASS(IC) ->
    splice_in_class(nword_items(Mode, CI), Rest, Mode, CI, MS);
translate_pat([$\\, D | Rest], IC, Mode, CI, MS)
  when ?IN_CLASS(IC), D =:= $d orelse D =:= $D ->
    splice_in_class([$\\, D], Rest, Mode, CI, MS);
translate_pat([$-, $\\, E | Rest], IC, Mode, CI, MS)
  when ?IN_CLASS(IC),
       E =:= $d orelse E =:= $D orelse E =:= $s orelse E =:= $S
       orelse E =:= $w orelse E =:= $W ->
    [$\\, $- | translate_pat([$\\, E | Rest], true, Mode, CI, MS)];
translate_pat([$-, C | _] = L, atom, Mode, CI, MS) when C =/= $] ->
    translate_range_hi(tl(L), Mode, CI, MS);
translate_pat([$- | Rest], IC, Mode, CI, MS) when ?IN_CLASS(IC) ->
    [$\\, $- | translate_pat(Rest, atom, Mode, CI, MS)];
translate_pat([$\\, $b | Rest], false, Mode, CI, MS) ->
    W = word_atom(Mode),
    "(?:(?<=" ++ W ++ ")(?!" ++ W ++ ")|(?<!" ++ W ++ ")(?=" ++ W ++ "))"
        ++ translate_pat(Rest, false, Mode, CI, MS);
translate_pat([$\\, $B | Rest], false, Mode, CI, MS) ->
    W = word_atom(Mode),
    "(?:(?<=" ++ W ++ ")(?=" ++ W ++ ")|(?<!" ++ W ++ ")(?!" ++ W ++ "))"
        ++ translate_pat(Rest, false, Mode, CI, MS);
translate_pat([{pcre, Io} | Rest], InClass, Mode, CI, MS) ->
    [Io | translate_pat(Rest, after_atom(InClass), Mode, CI, MS)];
translate_pat([$\\, C | Rest], InClass, Mode, CI, MS)
  when C =:= $v; C =:= $a; C =:= $e; C =:= $g;
       C =:= $h; C =:= $H; C =:= $V; C =:= $R; C =:= $X; C =:= $N;
       C =:= $z; C =:= $Z; C =:= $A; C =:= $G; C =:= $C; C =:= $K ->
    ["\\x{", integer_to_list(js_escape_cp(C), 16), "}"
     | translate_pat(Rest, after_atom(InClass), Mode, CI, MS)];
translate_pat([$\\, C | Rest], InClass, Mode, CI, MS) ->
    [$\\, C | translate_pat(Rest, after_atom(InClass), Mode, CI, MS)];
translate_pat([$[ | Rest], false, v, CI, MS) ->
    case arc_regex_vclass:parse(Rest, CI) of
        {ok, Ranges0, Strings, Rest2} ->
            Ranges = case CI of
                         true -> ?CS:vclose(Ranges0);
                         false -> Ranges0
                     end,
            ?CS:emit_vclass(Ranges, Strings) ++ translate_pat(Rest2, false, v, CI, MS);
        error ->
            open_class(Rest, v, CI, MS)
    end;
translate_pat([$[ | Rest], false, Mode, CI, MS) ->
    open_class(Rest, Mode, CI, MS);
translate_pat([$] | Rest], IC, Mode, CI, MS) when ?IN_CLASS(IC) ->
    [$] | translate_pat(Rest, false, Mode, CI, MS)];
translate_pat([$( | Rest], false, Mode, CI, [Cur | _] = MS) ->
    case take_modifiers(Rest, Cur) of
        {ok, Src, Cur2, Rest2} ->
            [$( | Src] ++ translate_pat(Rest2, false, Mode, CI, [Cur2 | MS]);
        none ->
            [$( | translate_pat(Rest, false, Mode, CI, [Cur | MS])]
    end;
translate_pat([$) | Rest], false, Mode, CI, MS) ->
    [$) | translate_pat(Rest, false, Mode, CI, pop_ms(MS))];
translate_pat([$. | Rest], false, Mode, CI, [{_M, false} | _] = MS) ->
    "[^" ?JS_LT "]" ++ translate_pat(Rest, false, Mode, CI, MS);
translate_pat([$. | Rest], false, Mode, CI, [{_M, true} | _] = MS) ->
    "(?s:.)" ++ translate_pat(Rest, false, Mode, CI, MS);
translate_pat([$^ | Rest], false, Mode, CI, [{false, _S} | _] = MS) ->
    "\\A" ++ translate_pat(Rest, false, Mode, CI, MS);
translate_pat([$^ | Rest], false, Mode, CI, [{true, _S} | _] = MS) ->
    "(?:\\A|(?<=[" ?JS_LT "]))" ++ translate_pat(Rest, false, Mode, CI, MS);
translate_pat([$$ | Rest], false, Mode, CI, [{false, _S} | _] = MS) ->
    "\\z" ++ translate_pat(Rest, false, Mode, CI, MS);
translate_pat([$$ | Rest], false, Mode, CI, [{true, _S} | _] = MS) ->
    "(?=[" ?JS_LT "]|\\z)" ++ translate_pat(Rest, false, Mode, CI, MS);
translate_pat([C | Rest], InClass, Mode, CI, MS) ->
    [C | translate_pat(Rest, after_atom(InClass), Mode, CI, MS)].

open_class(Rest, Mode, CI, MS) ->
    {Open, Body} = case Rest of
                       [$^ | R] -> {"[^", R};
                       _ -> {"[", Rest}
                   end,
    Open ++ translate_pat(Body, true, Mode, CI, MS).

after_atom(false) -> false;
after_atom(_InClass) -> atom.

after_class_item(false) -> false;
after_class_item(_InClass) -> true.

js_escape_cp($v) -> 16#0B;
js_escape_cp(C) -> C.

translate_range_hi([$\\, $u | R0] = L, Mode, CI, MS) ->
    Braced = case R0 of [${ | _] -> true; _ -> false end,
    case parse_uescape(R0) of
        {ok, V, R1} when V < 16#D800; V > 16#DFFF ->
            "-\\x{" ++ integer_to_list(V, 16) ++ "}"
                ++ translate_pat(R1, true, Mode, CI, MS);
        {ok, V, R1} ->
            case (not Braced) andalso Mode =/= none andalso V =< 16#DBFF
                andalso pair_trail(R1) of
                {ok, W, R2} ->
                    "-\\x{" ++ integer_to_list(combine_surrogates(V, W), 16) ++ "}"
                        ++ translate_pat(R2, true, Mode, CI, MS);
                _ ->
                    "-\\x{D7FF}" ++ translate_pat(R1, true, Mode, CI, MS)
            end;
        none ->
            range_hi_verbatim(L, Mode, CI, MS)
    end;
translate_range_hi(L, Mode, CI, MS) ->
    range_hi_verbatim(L, Mode, CI, MS).

range_hi_verbatim([$\\, $x, A, B | R] = L, Mode, CI, MS) ->
    case is_hex(A) andalso is_hex(B) of
        true -> [$-, $\\, $x, A, B | translate_pat(R, true, Mode, CI, MS)];
        false -> range_hi_escape(L, Mode, CI, MS)
    end;
range_hi_verbatim([$\\, $c, C | R] = L, Mode, CI, MS) ->
    case (C >= $a andalso C =< $z) orelse (C >= $A andalso C =< $Z) of
        true -> [$-, $\\, $c, C | translate_pat(R, true, Mode, CI, MS)];
        false -> range_hi_escape(L, Mode, CI, MS)
    end;
range_hi_verbatim(L, Mode, CI, MS) ->
    range_hi_escape(L, Mode, CI, MS).

range_hi_escape([$\\, C | R], Mode, CI, MS)
  when C =:= $v; C =:= $a; C =:= $e; C =:= $g;
       C =:= $h; C =:= $H; C =:= $V; C =:= $R; C =:= $X; C =:= $N;
       C =:= $z; C =:= $Z; C =:= $A; C =:= $G; C =:= $C; C =:= $K ->
    [$-, "\\x{", integer_to_list(js_escape_cp(C), 16), "}"
     | translate_pat(R, true, Mode, CI, MS)];
range_hi_escape([$\\, C | R], Mode, CI, MS) ->
    [$-, $\\, C | translate_pat(R, true, Mode, CI, MS)];
range_hi_escape([C | R], Mode, CI, MS) ->
    [$-, C | translate_pat(R, true, Mode, CI, MS)].

take_modifiers([$? | Rest], Cur) ->
    {Add, R1} = take_ims(Rest, []),
    case R1 of
        [$: | R2] ->
            {ok, [$? | Add] ++ ":", apply_ims(Cur, Add, []), R2};
        [$- | R1b] ->
            case take_ims(R1b, []) of
                {Rem, [$: | R2]} ->
                    {ok, [$? | Add] ++ [$- | Rem] ++ ":",
                     apply_ims(Cur, Add, Rem), R2};
                _ -> none
            end;
        _ -> none
    end;
take_modifiers(_Rest, _Cur) -> none.

take_ims([C | Rest], Acc) when C =:= $i; C =:= $m; C =:= $s ->
    take_ims(Rest, [C | Acc]);
take_ims(Rest, Acc) -> {lists:reverse(Acc), Rest}.

apply_ims({M, S}, Add, Rem) ->
    {ims_bit($m, Add, Rem, M), ims_bit($s, Add, Rem, S)}.

ims_bit(C, Add, Rem, Cur) ->
    case {lists:member(C, Add), lists:member(C, Rem)} of
        {true, _} -> true;
        {_, true} -> false;
        _ -> Cur
    end.

pop_ms([_Inner, Outer | Rest]) -> [Outer | Rest];
pop_ms([Bottom]) -> [Bottom].

%% a dash right after a class escape is literal, escape it
splice_in_class(Items, [$-, C | Rest], Mode, CI, MS) when C =/= $] ->
    Items ++ [$\\, $-] ++ translate_pat([C | Rest], true, Mode, CI, MS);
splice_in_class(Items, Rest, Mode, CI, MS) ->
    Items ++ translate_pat(Rest, true, Mode, CI, MS).

word_atom(none) -> "\\w";
word_atom(_UOrV) -> ?WORD.

nword_atom(none) -> "\\W";
nword_atom(_UOrV) -> ?NWORD.

word_items(none) -> "\\w";
word_items(_UOrV) -> ?WORD_BODY.

nword_items(none, _CI) -> "\\W";
nword_items(_UOrV, CI) -> ?CS:emit_complement(?CS:vword(), CI).

take_prop([$} | Rest], Acc) -> {lists:reverse(Acc), Rest};
take_prop([C | Rest], Acc)
  when (C >= $a andalso C =< $z); (C >= $A andalso C =< $Z);
       (C >= $0 andalso C =< $9); C =:= $_; C =:= $= ->
    take_prop(Rest, [C | Acc]);
take_prop(_, _Acc) -> none.

prop_translation(Payload, Negated, InClass, Mode) ->
    case binary:split(list_to_binary(Payload), <<"=">>) of
        [Name, Value] ->
            arc_regex_props_ffi:translate_pair(Name, Value, Negated, InClass);
        [Name] ->
            arc_regex_props_ffi:translate_lone(Name, Negated, InClass, Mode =:= v)
    end.

take_hex([C | Rest], Acc) ->
    case is_hex(C) of
        true -> take_hex(Rest, [C | Acc]);
        false -> {lists:reverse(Acc), [C | Rest]}
    end;
take_hex([], Acc) -> {lists:reverse(Acc), []}.

is_hex(C) ->
    (C >= $0 andalso C =< $9)
        orelse (C >= $a andalso C =< $f)
        orelse (C >= $A andalso C =< $F).

is_surrogate_hex(Hex) ->
    V = list_to_integer(Hex, 16),
    V >= 16#D800 andalso V =< 16#DFFF.

%% lone surrogates never match, emit an unmatchable stand-in
emit_surrogate(false, Rest, Mode, CI, MS) ->
    "(?!)" ++ translate_pat(Rest, false, Mode, CI, MS);
emit_surrogate(true, Rest, Mode, CI, MS) ->
    {Item, Rest2} = class_surrogate_item(Rest),
    Item ++ translate_pat(Rest2, true, Mode, CI, MS).

class_surrogate_item([$-, C | _] = Rest) when C =/= $] ->
    case class_range_hi(tl(Rest)) of
        {ok, Hi, Rest2} when Hi > 16#DFFF ->
            {"\\x{E000}-\\x{" ++ integer_to_list(Hi, 16) ++ "}", Rest2};
        {ok, _Hi, Rest2} ->
            {"\\p{Cs}", Rest2};
        none ->
            {"\\p{Cs}\\-", tl(Rest)}
    end;
class_surrogate_item(Rest) ->
    {"\\p{Cs}", Rest}.

class_range_hi([$\\, $u | R0]) -> parse_uescape(R0);
class_range_hi([C | R]) when C > 16#DFFF -> {ok, C, R};
class_range_hi(_) -> none.

parse_uescape([${ | R]) ->
    case take_hex(R, []) of
        {Hex, [$} | R2]} when Hex =/= [] -> {ok, list_to_integer(Hex, 16), R2};
        _ -> none
    end;
parse_uescape([A, B, C, D | R]) ->
    case is_hex(A) andalso is_hex(B) andalso is_hex(C) andalso is_hex(D) of
        true -> {ok, list_to_integer([A, B, C, D], 16), R};
        false -> none
    end;
parse_uescape(_) -> none.

combine_surrogates(Lead, Trail) ->
    16#10000 + (Lead - 16#D800) * 16#400 + (Trail - 16#DC00).

pair_trail([$\\, $u, E, F, G, H | Rest]) ->
    case is_hex(E) andalso is_hex(F) andalso is_hex(G) andalso is_hex(H) of
        true ->
            case list_to_integer([E, F, G, H], 16) of
                W when W >= 16#DC00, W =< 16#DFFF -> {ok, W, Rest};
                _ -> none
            end;
        false -> none
    end;
pair_trail(_) -> none.

regexp_exec_info(Pattern, Flags, String, Offset, Sticky) ->
    case check_offset(String, Offset) of
        {ok, Offset1} ->
            run_compiled(get_compiled(Pattern, Flags), String, Offset1, Sticky);
        {error, _} = Err ->
            Err
    end.

regexp_compile(Pattern, Flags) ->
    get_compiled(Pattern, Flags).

is_compiled({ok, {_MP, _GroupCount, _Names}}) -> true;
is_compiled({error, {pattern_compile_failed, _Reason}}) -> true;
is_compiled(_) -> false.

regexp_exec_compiled(Compiled, String, Offset, Sticky) ->
    case check_offset(String, Offset) of
        {ok, Offset1} -> run_compiled(Compiled, String, Offset1, Sticky);
        {error, _} = Err -> Err
    end.

%% byte offsets; mid-char offset is no_match, never badarg
check_offset(String, Offset) when Offset < 0 ->
    check_offset(String, 0);
check_offset(String, Offset) when Offset > byte_size(String) ->
    {error, offset_out_of_range};
check_offset(String, Offset) when Offset < byte_size(String) ->
    case (binary:at(String, Offset) band 16#C0) =:= 16#80 of
        true -> {error, no_match};
        false -> {ok, Offset}
    end;
check_offset(_String, Offset) ->
    {ok, Offset}.

run_compiled({error, {pattern_compile_failed, _Reason}} = Err, _S, _O, _St) ->
    Err;
run_compiled({ok, {MP, GroupCount, Names}}, String, Offset, Sticky) ->
    Opts0 = [{offset, Offset}, {capture, all, index}],
    Opts = case Sticky of
               true -> [anchored | Opts0];
               false -> Opts0
           end,
    case re:run(String, MP, Opts) of
        {match, [Whole | Groups]} ->
            Padded = pad_captures(Groups, GroupCount),
            {ok, {Whole, Padded, GroupCount, Names}};
        nomatch -> {error, no_match}
    end.

pad_captures(Caps, N) when length(Caps) >= N -> Caps;
pad_captures(Caps, N) -> Caps ++ lists:duplicate(N - length(Caps), {-1, 0}).

take_group_name(L) -> take_group_name(L, []).

take_group_name([$> | Rest], Acc) -> {lists:reverse(Acc), Rest, true};
take_group_name([C | Rest], Acc) -> take_group_name(Rest, [C | Acc]);
take_group_name([], Acc) -> {lists:reverse(Acc), [], false}.
